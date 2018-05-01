(load "classParser.scm")

; The main function.  Calls parser to get the parse tree and interprets it with a new environment.  The returned value is in the environment.
(define interpret
  (lambda (file class-name)
    (scheme->language
     (eval-funcall main-method (insert 'main (car (cadar (get-class-main class-name (interpret-class-list (parser "test.txt") (newenvironment))))) (interpret-class-list (parser "test.txt") (newenvironment))) invalid-throw invalid-current-type))))

(define interpret-class-list
  (lambda (statement-list environment)
    (cond
      ((null? statement-list) environment)
      ((eq? 'class (statement-type (car statement-list)))
       (interpret-class-list (cdr statement-list) (interpret-class (car statement-list) environment)))
      (else (myerror "Invalid class syntax"))))) ; add an error, something other than a class was found

(define interpret-class
  (lambda (statement environment)
    (insert (class-name statement)
            (build-closure-from-list (body statement) (initial-class-closure (parent-name statement)))
            environment)))

; interprets a list of statements.  The environment from each statement is used for the next ones.
(define interpret-statement-list
  (lambda (statement-list environment return break continue throw)
    (if (null? statement-list)
        environment
        (interpret-statement-list (cdr statement-list) (interpret-statement (car statement-list) environment return break continue throw) return break continue throw))))

; interpret a statement in the environment with continuations for return, break, continue, throw
(define interpret-statement
  (lambda (statement environment return break continue throw current-type)
    (cond
      ((eq? 'class (statement-type statement)) (interpret-class statement environment current-type))
      ((eq? 'return (statement-type statement)) (interpret-return statement environment return throw current-type))
      ((eq? 'function (statement-type statement)) (interpret-function (add-this statement) environment current-type))
      ((eq? 'funcall (statement-type statement)) (interpret-funcall (eval-funcall statement environment throw current-type) environment current-type))
      ((eq? 'var (statement-type statement)) (interpret-declare statement environment throw current-type))
      ((eq? '= (statement-type statement)) (interpret-assign statement environment throw current-type))
      ((eq? 'if (statement-type statement)) (interpret-if statement environment return break continue throw current-type))
      ((eq? 'while (statement-type statement)) (interpret-while statement environment return throw current-type))
      ((eq? 'continue (statement-type statement)) (continue environment current-type))
      ((eq? 'break (statement-type statement)) (break environment current-type))
      ((eq? 'begin (statement-type statement)) (interpret-block statement environment return break continue throw current-type))
      ((eq? 'throw (statement-type statement)) (interpret-throw statement environment throw current-type))
      ((eq? 'try (statement-type statement)) (begin (set-box! throwenv environment)(interpret-try statement environment return break continue throw current-type)))
      (else (myerror "Unknown statement:" (statement-type statement))))))

; Calls the return continuation with the given expression value
(define interpret-return
  (lambda (statement environment return throw current-type)
    (return (eval-expression (get-expr statement) environment throw))))

; Adds the function binding to the environment
(define interpret-function
  (lambda (statement environment current-type)
    (insert (get-function-name statement) (get-function-closure statement) environment)))

; Executes the function then returns the state
(define interpret-funcall
  (lambda (function environment)
    environment))

; Evaluates a function and returns the value
(define eval-funcall
  (lambda (statement environment throw current-type)
    (call/cc
      (lambda (return)
        (interpret-statement-list (function-body statement environment) (push-function-frame statement environment throw) return invalid-break invalid-continue throw)))))

(define eval-dot
  (lambda (statement environment throw current-type)
    (eval-expression (lookup-in-instance (caddr statement) (cadr statement) environment) environment throw)))

; Adds a new variable binding to the environment.  There may be an assignment with the variable
(define interpret-declare
  (lambda (statement environment throw current-type)
    (if (exists-declare-value? statement)
        (insert (get-declare-var statement) (box (eval-expression (get-declare-value statement) environment throw)) environment)
        (insert (get-declare-var statement) (box 'novalue) environment))))

; Updates the environment to add an new binding for a variable
(define interpret-assign
  (lambda (statement environment throw current-type)
    (update (get-assign-lhs statement) (eval-expression (get-assign-rhs statement) environment throw) environment)))

; We need to check if there is an else condition.  Otherwise, we evaluate the expression and do the right thing.
(define interpret-if
  (lambda (statement environment return break continue throw current-type)
    (cond
      ((eval-expression (get-condition statement) environment throw) (interpret-statement (get-then statement) environment return break continue throw))
      ((exists-else? statement) (interpret-statement (get-else statement) environment return break continue throw))
      (else environment))))


; Interprets a while loop.  We must create break and continue continuations for this loop
(define interpret-while
  (lambda (statement environment return throw current-type)
    (call/cc
     (lambda (break)
       (letrec ((loop (lambda (condition body environment)
                        (if (eval-expression condition environment throw)
                            (loop condition body (interpret-statement body environment return break (lambda (env) (break (loop condition body env))) throw))
                         environment))))
         (loop (get-condition statement) (get-body statement) environment))))))

; Interprets a block.  The break, continue, and throw continuations must be adjusted to pop the environment
(define interpret-block
  (lambda (statement environment return break continue throw current-type)
    (pop-frame (interpret-statement-list (cdr statement)
                                         (push-frame environment)
                                         return
                                         (lambda (env) (break (pop-frame env)))
                                         (lambda (env) (continue (pop-frame env)))
                                         (lambda (v env) (throw v (pop-frame env)))))))

; We use a continuation to throw the proper value. Because we are not using boxes, the environment/state must be thrown as well so any environment changes will be kept
(define interpret-throw
  (lambda (statement environment throw current-type)
    (throw (box (eval-expression (get-expr statement) environment throw)) (push-frame (unbox throwenv)))))

; Interpret a try-catch-finally block

; Create a continuation for the throw.  If there is no catch, it has to interpret the finally block, and once that completes throw the exception.
;   Otherwise, it interprets the catch block with the exception bound to the thrown value and interprets the finally block when the catch is done
(define create-throw-catch-continuation
  (lambda (catch-statement environment return break continue throw jump finally-block)
    (cond
      ((null? catch-statement) (lambda (ex env) (throw ex (interpret-block finally-block env return break continue throw))))
      ((not (eq? 'catch (statement-type catch-statement))) (myerror "Incorrect catch statement"))
      (else (lambda (ex env)
              (jump (interpret-block finally-block
                                     (pop-frame (interpret-statement-list
                                                 (get-body catch-statement)
                                                 (insert (catch-var catch-statement) ex (push-frame env))
                                                 return
                                                 (lambda (env2) (break (pop-frame env2)))
                                                 (lambda (env2) (continue (pop-frame env2)))
                                                 (lambda (v env2) (throw v (pop-frame env2)))))
                                     return break continue throw)))))))

; To interpret a try block, we must adjust  the return, break, continue continuations to interpret the finally block if any of them are used.
;  We must create a new throw continuation and then interpret the try block with the new continuations followed by the finally block with the old continuations
(define interpret-try
  (lambda (statement environment return break continue throw current-type)
    (call/cc
     (lambda (jump)
       (let* ((finally-block (make-finally-block (get-finally statement)))
              (try-block (make-try-block (get-try statement)))
              (new-return (lambda (v) (begin (interpret-block finally-block environment return break continue throw) (return v))))
              (new-break (lambda (env) (break (interpret-block finally-block env return break continue throw))))
              (new-continue (lambda (env) (continue (interpret-block finally-block env return break continue throw))))
              (new-throw (create-throw-catch-continuation (get-catch statement) environment return break continue throw jump finally-block)))(interpret-block finally-block
                          (interpret-block try-block environment new-return new-break new-continue new-throw)
                          return break continue throw))))))

; helper methods so that I can reuse the interpret-block method on the try and finally blocks
(define make-try-block
  (lambda (try-statement)
    (cons 'begin try-statement)))

(define make-finally-block
  (lambda (finally-statement)
    (cond
      ((null? finally-statement) '(begin))
      ((not (eq? (statement-type finally-statement) 'finally)) (myerror "Incorrectly formatted finally block"))
      (else (cons 'begin (cadr finally-statement))))))

; Evaluates all possible boolean and arithmetic expressions, including constants and variables.
(define eval-expression
  (lambda (expr environment throw)
    (cond
      ((number? expr) expr)
      ((eq? expr 'true) #t)
      ((eq? expr 'false) #f)
      ((not (list? expr)) (unbox (lookup expr environment)))
      ((eq? 'dot (operator expr)) (eval-dot expr environment throw))
      ((eq? (statement-type expr) 'new) (build-instance-closure expr environment))
      ((eq? 'funcall (operator expr)) (eval-funcall expr environment throw)) ; interpret-funcall is not implemented yet
      (else (eval-operator expr environment throw)))))

(define eval-operator
  (lambda (expr environment throw)
    (cond
      ((eq? '! (operator expr)) (not (eval-expression (operand1 expr) environment throw)))
      ((and (eq? '- (operator expr)) (= 2 (length expr))) (- (eval-expression (operand1 expr) environment throw)))
      (else (eval-binary-op2 expr (eval-expression (operand1 expr) environment throw) environment throw)))))

; Complete the evaluation of the binary operator by evaluating the second operand and performing the operation.
(define eval-binary-op2
  (lambda (expr op1value environment throw)
    (cond
      ((eq? '+ (operator expr)) (+ op1value (eval-expression (operand2 expr) environment throw)))
      ((eq? '- (operator expr)) (- op1value (eval-expression (operand2 expr) environment throw)))
      ((eq? '* (operator expr)) (* op1value (eval-expression (operand2 expr) environment throw)))
      ((eq? '/ (operator expr)) (quotient op1value (eval-expression (operand2 expr) environment throw)))
      ((eq? '% (operator expr)) (remainder op1value (eval-expression (operand2 expr) environment throw)))
      ((eq? '== (operator expr)) (isequal op1value (eval-expression (operand2 expr) environment throw)))
      ((eq? '!= (operator expr)) (not (isequal op1value (eval-expression (operand2 expr) environment throw))))
      ((eq? '< (operator expr)) (< op1value (eval-expression (operand2 expr) environment throw)))
      ((eq? '> (operator expr)) (> op1value (eval-expression (operand2 expr) environment throw)))
      ((eq? '<= (operator expr)) (<= op1value (eval-expression (operand2 expr) environment throw)))
      ((eq? '>= (operator expr)) (>= op1value (eval-expression (operand2 expr) environment throw)))
      ((eq? '|| (operator expr)) (or op1value (eval-expression (operand2 expr) environment throw)))
      ((eq? '&& (operator expr)) (and op1value (eval-expression (operand2 expr) environment throw)))
      (else (myerror "Unknown operator:" (operator expr))))))

; Determines if two values are equal.  We need a special test because there are both boolean and integer types.
(define isequal
  (lambda (val1 val2)
    (if (and (number? val1) (number? val2))
        (= val1 val2)
        (eq? val1 val2))))


;-----------------
; HELPER FUNCTIONS
;-----------------

; These helper functions define the operator and operands of a value expression
(define operator car)
(define operand1 cadr)
(define operand2 caddr)
(define operand3 cadddr)

(define exists-operand2?
  (lambda (statement)
    (not (null? (cddr statement)))))

(define exists-operand3?
  (lambda (statement)
    (not (null? (cdddr statement)))))

(define throwenv (box '()))

; these helper functions define the parts of the various statement types
(define get-class-name operand1)
(define get-class-main
  (lambda (class-name env)
    (cdr (lookup class-name env))))
(define get-class-closure operand3)
(define statement-type operator)
(define get-expr operand1)
(define get-function-name operand1)
(define get-function-closure
  (lambda (statement)
    (cons (operand2 statement) (cons (operand3 statement) '()))))
(define get-declare-var operand1)
(define get-declare-value operand2)
(define exists-declare-value? exists-operand2?)
(define get-assign-lhs operand1)
(define get-assign-rhs operand2)
(define get-condition operand1)
(define get-then operand2)
(define get-else operand3)
(define get-body operand2)
(define exists-else? exists-operand3?)
(define get-try operand1)
(define get-catch operand2)
(define get-finally operand3)

(define catch-var
  (lambda (catch-statement)
    (car (operand1 catch-statement))))

;;;;;;;;;;;;;;;;;;;;;;;;;
; Function helper methods
;;;;;;;;;;;;;;;;;;;;;;;;;

; Defines the main method call
(define main-method '(funcall main))

(define determine-function-class
  (lambda (instance-closure function-name environment)
    (function-class-from-closure (lookup (instance-class instance-closure) environment) (instance-class instance-closure) function-name environment)))

(define function-class-from-closure
  (lambda (class-closure current-class function-name environment)
    (find-function-class (class-parent class-closure) current-class (class-function-names class-closure) function-name environment)))

(define find-function-class
  (lambda (parent-class current-class function-list function-name environment)
    (cond
      ((null? function-list) (update-finder-to-closure parent-class function-name environment))
      ((eq? (car function-list) function-name) current-class)
      (else (find-function-class parent-class current-class (cdr function-list) function-name environment)))))

(define update-finder-to-closure
  (lambda (current-class function-name environment)
    (if (null? current-class)
        (myerror "Function not found")
        (update-function-finder (lookup current-class environment) current-class function-name environment))))

(define update-function-finder
  (lambda (class-closure current-class function-name environment)
    (if (null? current-class)
        (myerror "Function not found")
        (find-function-class (class-parent class-closure) current-class (class-function-names class-closure) function-name environment))))

(define function-body
  (lambda (statement environment)
    (cadr (function-closure statement environment))))
    
(define function-params
  (lambda (statement environment)
    (car (function-closure statement environment))))

(define function-closure
  (lambda (statement environment)
    (find-function-closure (find-true-type (get-dot-expr statement) environment) (func-name (get-dot-expr statement)) environment)))

(define find-true-type
  (lambda (dot-expr environment)
    (determine-function-class (find-instance-closure (instance dot-expr) environment) (func-name dot-expr) environment)))

(define find-function-closure
  (lambda (true-type function-name environment)
    (lookup-in-frame function-name (class-function (lookup true-type environment)))))

(define find-instance-closure
  (lambda (instance environment)
    (cond
      ((not (list? instance)) (lookup instance environment))
      ((eq? 'new (car instance)) (build-instance-closure instance environment))
      (else myerror "Idk if this can happen"))))

(define get-dot-expr cadr)
(define instance cadr)
(define func-name caddr)

(define parameters
  (lambda (statement)
    (cddr statement)))

; Evaluates the parameters
(define parameter-values
  (lambda (parameters environment throw)
    (if (null? parameters)
         '()
         (cons (box (eval-expression (car parameters) environment throw)) (parameter-values (cdr parameters) environment throw)))))

; Creates frame where the names are parameter names and values are parameter values
(define function-frame
  (lambda (statement environment throw)
    (if (matching-parameters? (function-params statement environment) (parameter-values (parameters statement) environment throw))
      (cons (function-params statement environment) (cons (parameter-values (parameters statement) environment throw) '()))
      (myerror "Mismatched paramters"))))

; Gets the static link for a function
(define get-static-link
  (lambda (name environment)
    (cond
      ((null? environment) (myerror "function not found: " name))
      ((and (list? name) (null? (cdr environment))) environment)
      ((exists-in-list? name (caar environment)) environment)
      (else (get-static-link name (pop-frame environment))))))

; Verifies that the parameter count matches the function call
(define matching-parameters?
  (lambda (names values)
    (if (nor (null? names) (null? values))
        (matching-parameters? (cdr names) (cdr values))
        (and (null? names) (null? values)))))

; Returns the function frame and the environment of the static link
(define push-function-frame
  (lambda (statement environment throw)
    (cons (function-frame statement environment throw) (get-static-link (cadr statement) environment))))

;------------------------
; Class Closure Functions
;------------------------

(define build-closure-from-list
  (lambda (statement-list class-closure)
    (if (null? statement-list)
      class-closure
      (build-closure-from-list (cdr statement-list) (build-closure (car statement-list) class-closure)))))

(define build-closure
  (lambda (statement class-closure)
    (cond
      ((or (eq? 'static-var (statement-type statement)) (eq? 'var (statement-type statement)))
       (add-var-to-closure statement class-closure))
      ((or (eq? 'static-function (statement-type statement)) (eq? 'function (statement-type statement)))
       (add-function-to-closure statement class-closure))
      (else (myerror "Invalid syntax in class")))))

(define add-var-to-closure
  (lambda (statement class-closure)
    (if (exists-in-list? (var-name statement) (class-variable-names class-closure))
      (myerror "Instance variable declared twice.")
      (list (class-parent class-closure)
            (class-function class-closure)
            (cons (var-name statement) (class-variable-names class-closure))
            (add-var-val statement class-closure)))))

(define add-var-val
  (lambda (statement class-closure)
    (if (has-var-value? statement)
      (cons (var-val statement) (class-default-values class-closure))
      (cons 'novalue (class-default-values class-closure)))))

(define add-function-to-closure
  (lambda (statement class-closure)
    (if (exists-in-list? (get-function-name statement) (class-function-names class-closure))
      (myerror "Function declared twice.")
      (list (class-parent class-closure) 
            (add-to-frame (get-function-name statement) (get-function-closure statement) (class-function class-closure))
            (class-variable-names class-closure)
            (class-default-values class-closure)))))

(define has-var-value?
  (lambda (statement)
    (if (null? (cddr statement))
      #f
      #t)))
(define var-val caddr)
(define var-name cadr)
(define class-name cadr)
(define body cadddr)

(define parent-name
  (lambda (statement)
    (if (null? (caddr statement))
      '()
      (cadr (caddr statement)))))

(define initial-class-closure
  (lambda (parent-name)
    (list parent-name (newframe) '() '())))

(define class-parent
  (lambda (class)
    (car class)))

(define class-function-names
  (lambda (class)
    (car (class-function class))))

(define class-method-closure
  (lambda (class)
    (cadr (class-function class))))

(define class-function
  (lambda (class)
    (cadr class)))

(define class-variable-names
  (lambda (class)
    (caddr class)))

(define class-default-values
  (lambda (class)
    (cadddr class)))

;--------------------------
; Instance Closure Function
;--------------------------

(define build-instance-closure
  (lambda (expr environment)
    (list (get-class-name expr) (class-default-values (lookup (get-class-name expr) environment)))))

(define instance-closure
  (lambda (class instance-values)
    (list class instance-values)))

(define instance-class
  (lambda (instance)
    (car instance)))

(define instance-values
  (lambda (instance)
    (cadr instance)))

;----------------------------
; Environment/State Functions
;----------------------------

; create a new empty environment
(define newenvironment
  (lambda ()
    (list (newframe))))

; create an empty frame: a frame is two lists, the first are the variables and the second is the "store" of values
(define newframe
  (lambda ()
    '(() ())))

; add a frame onto the top of the environment
(define push-frame
  (lambda (environment)
    (cons (newframe) environment)))

; remove a frame from the environment
(define pop-frame
  (lambda (environment)
    (cdr environment)))

; some abstractions
(define topframe car)
(define remainingframes cdr)

; does a variable exist in the environment?
(define exists?
  (lambda (var environment)
    (cond
      ((null? environment) #f)
      ((exists-in-list? var (variables (topframe environment))) #t)
      (else (exists? var (remainingframes environment))))))

; does a variable exist in a list?
(define exists-in-list?
  (lambda (var l)
    (cond
      ((null? l) #f)
      ((eq? var (car l)) #t)
      (else (exists-in-list? var (cdr l))))))

; Looks up a value in the environment.  If the value is a boolean, it converts our languages boolean type to a Scheme boolean type
(define lookup
  (lambda (var environment)
    (lookup-variable var environment)))

; A helper function that does the lookup.  Returns an error if the variable does not have a legal value
(define lookup-variable
  (lambda (var environment)
    (let ((value (lookup-in-env var environment)))
      (if (eq? 'novalue value)
          (myerror "error: variable without an assigned value:" var)
          value))))

;;;; Lookup, Declare, and Update non-static fields ;;;;

(define exists-in-class?
  (lambda (var class-name environment)
    (exists-in-class-closure? var (lookup class-name environment) environment)))

(define exists-in-class-closure?
  (lambda (var class-closure environment)
    (if (exists-in-list? var (class-variable-names class-closure))
        #t
        (exists-in-parent? var class-closure environment))))

(define exists-in-parent?
  (lambda (var class-closure environment)
    (if (null? (class-parent class-closure))
        #f
        (exists-in-class? var (class-parent class-closure) environment))))

; Lookup method
(define lookup-in-instance
  (lambda (var instance-name environment)
    (if (exists-in-class? var (instance-class (unbox (lookup instance-name environment))) environment)
        (value-in-instance var (unbox (lookup instance-name environment)) environment)
        (myerror "Field name does not exist in instance"))))

(define value-in-instance
  (lambda (var instance-closure environment)
    (value-in-closure var (lookup (instance-class instance-closure) environment) instance-closure environment)))

(define value-in-closure
  (lambda (var class-closure instance-closure environment)
    (value-in-heirarchy var (class-parent class-closure) (class-variable-names class-closure) (instance-values instance-closure) environment)))

(define value-in-heirarchy
  (lambda (var parent-name variable-names variable-values environment)
    (cond
      ((null? variable-names)
       (value-in-heirarchy var (class-parent (lookup parent-name environment)) (class-variable-names (lookup parent-name environment)) instance-values environment))
      ((eq? var (car variable-names)) (car variable-values))
      (else (value-in-heirarchy var parent-name (cdr variable-names) (cdr variable-values) environment)))))

; Update method
(define update-in-instance
  (lambda (var val instance-name environment)
    (update instance-name (update-instance-closure var val (unbox (lookup instance-name environment)) environment) environment)))

(define update-instance-closure
 (lambda (var val instance-closure environment)
   (if (exists-in-class? var (instance-class instance-closure) environment)
       (list (instance-class instance-closure)
             (update-instance-value var val (lookup (instance-class instance-closure) environment) instance-closure environment))
       (myerror "Field name does not exist in instance"))))

(define update-instance-value
 (lambda (var val class-closure instance-closure environment)
   (update-instance-value-w-parent var val (class-parent class-closure) (class-variable-names class-closure) (instance-values instance-closure) environment)))

(define update-instance-value-w-parent
  (lambda (var val parent-name names values environment)
    (cond
      ((null? names)
       (update-instance-value-w-parent var val (class-parent (lookup parent-name environment)) (class-variable-names (lookup parent-name environment)) values environment))
      ((eq? var (car names)) (cons val (cdr values)))
      (else (cons (car values) (update-instance-value-w-parent var val parent-name (cdr names) (cdr values) environment))))))

; Return the value bound to a variable in the environment
(define lookup-in-env
  (lambda (var environment)
    (cond
      ((null? environment) (myerror "error: undefined variable" var))
      ((exists-in-list? var (variables (topframe environment))) (lookup-in-frame var (topframe environment)))
      (else (lookup-in-env var (cdr environment))))))

; Return the value bound to a variable in the frame
(define lookup-in-frame
  (lambda (var frame)
    (cond
      ((not (exists-in-list? var (variables frame))) (myerror "error: undefined variable" var))
      (else (language->scheme (get-value (indexof var (variables frame)) (store frame)))))))

; Get the location of a name in a list of names
(define indexof
  (lambda (var l)
    (cond
      ((null? l) 0)  ; should not happen
      ((eq? var (car l)) 0)
      (else (+ 1 (indexof var (cdr l)))))))

; Get the value stored at a given index in the list
(define get-value
  (lambda (n l)
    (cond
      ((zero? n) (car l))
      (else (get-value (- n 1) (cdr l))))))

; Adds a new variable/value binding pair into the environment.  Gives an error if the variable already exists in this frame.
(define insert
  (lambda (var val environment)
    (if (exists-in-list? var (variables (car environment)))
        (myerror "error: variable is being re-declared:" var)
        (cons (add-to-frame var val (car environment)) (cdr environment)))))

; Changes the binding of a variable to a new value in the environment.  Gives an error if the variable does not exist.
(define update
  (lambda (var val environment)
    (if (exists? var environment)
        (update-existing var val environment)
        (myerror "error: variable used but not defined:" var))))

; Add a new variable/value pair to the frame.
(define add-to-frame
  (lambda (var val frame)
    (list (cons var (variables frame)) (cons (scheme->language val) (store frame)))))

; Changes the binding of a variable in the environment to a new value
(define update-existing
  (lambda (var val environment)
    (if (exists-in-list? var (variables (car environment)))
        (cons (update-in-frame var val (topframe environment)) (remainingframes environment))
        (cons (topframe environment) (update-existing var val (remainingframes environment))))))

; Changes the binding of a variable in the frame to a new value.
(define update-in-frame
  (lambda (var val frame)
    (list (variables frame) (update-in-frame-store var val (variables frame) (store frame)))))

; Changes a variable binding by placing the new value in the appropriate place in the store
(define update-in-frame-store
  (lambda (var val varlist vallist)
    (cond
      ((eq? var (car varlist)) (begin (set-box! (car vallist) (scheme->language val)) vallist))
      (else (cons (car vallist) (update-in-frame-store var val (cdr varlist) (cdr vallist)))))))

; Returns the list of variables from a frame
(define variables
  (lambda (frame)
    (car frame)))

; Returns the store from a frame
(define store
  (lambda (frame)
    (cadr frame)))

; Functions to convert the Scheme #t and #f to our languages true and false, and back.

(define language->scheme
  (lambda (v)
    (cond
      ((eq? v 'false) #f)
      ((eq? v 'true) #t)
      (else v))))

(define scheme->language
  (lambda (v)
    (cond
      ((eq? v #f) 'false)
      ((eq? v #t) 'true)
      (else v))))

;;;;;;;;;;;;;;;;;;;
; ERROR FUNCTIONS ;
;;;;;;;;;;;;;;;;;;;

(define invalid-current-type
  (lambda (env)
    (myerror "invalid current type")))

(define invalid-return
  (lambda (env)
    (myerror "Return called outside a function")))

(define invalid-break
  (lambda (env)
    (myerror "Break used outside of loop")))

(define invalid-continue
  (lambda (env)
    (myerror "Continue used outside of loop")))

(define invalid-throw
  (lambda (v env)
    (myerror "Uncaught exception thrown")))

; Because the error function is not defined in R5RS scheme, I create my own:
(define error-break (lambda (v) v))
  (call-with-current-continuation (lambda (k) (set! error-break k)))

(define myerror
  (lambda (str . vals)
    (letrec ((makestr (lambda (str vals)
                        (if (null? vals)
                            str
                            (makestr (string-append str (string-append " " (symbol->string (car vals)))) (cdr vals))))))
      (error-break (display (string-append str (makestr "" vals)))))))

(eval-funcall '(funcall (dot (new A) set2) 3 5) (interpret-class-list (parser "test.txt") (newenvironment)) '())
