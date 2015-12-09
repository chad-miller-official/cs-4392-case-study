;;; //////////////////////////
;;; Environment functions
;;; //////////////////////////

(define empty-environment '())

(define (make-frame vars vals)
  (cons vars vals))

(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
    (cons (make-frame vars vals) base-env)
    (if (< (length vars) (length vals))
      (error "Too many arguments supplied" vars vals)
      (error "Too few arguments supplied" vars vals))))

(define (set-var-val! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
	     (env-loop (cdr env)))
	    ((eq? var (car vars))
	     (set-car! vals val))
	    (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env empty-environment)
      (error "Unbound variable" var)
      (let ((frame (car env)))
	(scan (car frame)
	      (cdr frame)))))
  (env-loop env))


(define (define-var! var val env)
  (let ((frame (car env)))
    (define (scan vars vals)
      (cond ((null? vars)
	     (add-binding-to-frame! var val frame))
	    ((eq? var (car vars))
	     (set-car! vals val))
	    (else (scan (cdr vars) (cdr vals)))))
    (scan (car frame)
	  (cdr frame))
    var))

(define (lookup-var var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
	     (env-loop (cdr env)))
	    ((eq? var (car vars))
	     (car vals))
	    (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env empty-environment)
      (error "Unbound variable" var)
      (let ((frame (car env)))
	(scan (car frame)
	      (cdr frame)))))
  (env-loop env))

(define (setup-environment)
  (let ((initial-env
	  (extend-environment (list 'dummy)
			      (list 5)
			      '())))
    initial-env))

(define global-environment (setup-environment))


;;; One of two main entry points to metacircular evaluator. Evaluates
;;; expressions. 
(define (meval expr env)
  (cond ((self-evaluating? expr) expr)
        ((variable? expr) (meval-lookup-var expr env))
        ((quote? expr) (cadr expr))
	((assignment? expr) (meval-assignment expr env))
	((define? expr) (meval-define 
			  (definition-variable expr)
			  (definition-value expr)
			  env))
        ((if? expr) (meval-if (cdr expr) env))
        ((lambda? expr) (meval-lambda 
			  (lambda-params expr)
			  (lambda-body expr)
			  env))
        ((begin? expr) (meval-sequence (cdr expr) env))
        ((cond? expr) (meval-cond expr env))
        ((apply? expr) (mapply 
			 (car expr)
			 (list-of-eval-args (cdr expr) env)))
        (else (error "eval failed/not implemented\n"))))


;;; Other main entry point into metacircular evaluator. Applys arguments
;;; to a procedures.
(define (mapply procedure arguments)
  (cond ((primitive-procedure? procedure)
	 (apply-primitive-procedure procedure arguments))
	((compound-procedure? procedure)
	 (apply-compound-procedure procedure arguments))
	(else
	  (error "Invalid procedure"))))

;;; //////////////////////////
;;; Eval's condition checks
;;; //////////////////////////

;;; Determines if the expression is already in a form that can be evaluated.
;;; i.e. single numerics, strings, etc.
(define (self-evaluating? expr)
  (cond ((number? expr) #t)
        ((string? expr) #t)
        ((boolean? expr) #t)
        (else (char? expr))))

;;; Determines if the expression is a variable. A variable is just a symbol.
(define (variable? expr)
  (symbol? expr))

;;; A quoted expression just starts with the symbol "quote", or "'".
(define (quote? expr)
  (starts-with-symbol? expr 'quote))

(define (assignment? expr)
  (starts-with-symbol? expr 'set!))

(define (define? expr)
  (starts-with-symbol? expr 'define))

(define (if? expr)
  (starts-with-symbol? expr 'if))

(define (lambda? expr)
  (starts-with-symbol? expr 'lambda))

(define (begin? expr)
  (starts-with-symbol? expr 'begin))

(define (cond? expr)
  (starts-with-symbol? expr 'cond))

;;; Checks to see if it is a function application. If it does not match
;;; the other definitions and is still a pair, it is a function application.
(define (apply? expr)
  (pair? expr))

;;; Helper methods to check if expressions start with specific symbols
;;; Checks if the given expression begins with specified quoted symbol,
;;; i.e. begins with ', (starts-with-symbol? (+ 1 2) '+) == #t
(define (starts-with-symbol? expr symbol)
  (if (pair? expr)
    (eq? (car expr) symbol)
    #f))

;;; //////////////////////////
;;; Eval code
;;; //////////////////////////

;;; Given an environment, looks up a symbol. If the variable does
;;; not exist, it throws an error.
(define (meval-lookup-var symbol env)
  (lookup-var symbol env))

(define (meval-assignment expr env)
  (set-var-val! (cadr expr) (meval (caddr expr) env) env))

(define (meval-define symbol expr env)
   (define-var! symbol (meval expr env) env))

;;; (define x 5)
;;; (define (square x) (* x x))
(define (definition-variable expr)
  (if (symbol? (cadr expr))
    (cadr expr)
    (caadr expr)))

(define (definition-value expr)
  (if (symbol? (cadr expr))
    (caddr expr)
    (make-lambda (cdr (car (cdr expr)))
		 (cdr (cdr expr)))))

;;; Given an if statement and an environment, evaluates the if statement.
(define (meval-if expr env)
  (let ((condition (car expr))
	(then-stmt (cadr expr))
	(else-stmt (caddr expr)))
    (if (true? (meval condition env))
      (meval then-stmt env)
      (if (null? else-stmt) (meval else-stmt env)))))

(define (make-if condition then-stmt else-stmt)
  (list 'if condition then-stmt else-stmt))

(define (meval-lambda params body env)
  (make-procedure params body env))

(define (make-lambda param body)
  (cons 'lambda (cons param body)))

(define (lambda-params expr)
  (cadr expr))

(define (lambda-body expr)
  (cddr expr))

(define (meval-sequence exprs env)
  (cond ((null? (cdr exprs)) (meval (car exprs) env))
	(else (meval (car exprs) env)
	      (meval-sequence (cdr exprs) env))))

(define (sequence->expr seq)
  (cond ((null? seq) seq)
	((null? (cdr seq)) (car seq))
	(else (make-begin seq))))

(define (make-begin seq) (cons 'begin seq))

(define (meval-cond expr env)
  (meval (cond->if expr) env))

(define (cond->if expr)
  (expand-clauses (cdr expr)))

(define (expand-clauses clauses)
  (if (null? clauses)
    #f
    (let ((first (car clauses))
	  (rest (cdr clauses)))
      (if (eq? (car first) 'else)
	(if (null? rest)
	  (sequence->exp (cdr first))
	  (error "else clause should come last, but doesn't"))
	(make-if (car first)
		 (sequence->exp (cdr first))
		 (expand-clauses rest))))))

;;; Given the arguments of a function, evaluates them to their most basic
;;; level and puts them in a list.
(define (list-of-eval-args exprs env)
  (if (null? exprs)
    '()
    (cons (meval (car exprs) env) (list-of-eval-args (cdr exprs) env))))

;;; //////////////////////////
;;; Apply checks
;;; //////////////////////////

(define (primitive-procedure? proc)
  (cond ((eq? proc 'car) #t)
        ((eq? proc 'cdr) #t)
        ((eq? proc 'cons) #t)
        ((eq? proc 'null?) #t)
	((eq? proc '+) #t)
	((eq? proc '-) #t)
	((eq? proc '*) #t)
	((eq? proc '/) #t)
	((eq? proc 'list) #t)
	((eq? proc 'eq?) #t)
	((eq? proc '=) #t)
	(else #f)))

;(define (retrieve-procedure proc env)
;  (if (null? env)
;    (error "Procedure not found"))
;  (if (eq

(define (compound-procedure? proc)
  (
  (let ((result (lookup-var proc global-environment)))
    (starts-with-symbol? result 'procedure)))


;;; //////////////////////////
;;; Apply Code
;;; //////////////////////////

(define (apply-primitive-procedure proc args)
  (apply-in-scheme (cons proc args)))

(define (apply-in-scheme expr)
  (let ((procedure (car expr))
	(args (cdr expr)))
    (cond ((eq? procedure 'cons) (cons (car args) (cadr args)))
	  ((eq? procedure 'car) (car (car args)))
	  ((eq? procedure 'cdr) (cdr (car args)))
	  ((eq? procedure 'null?) (null? (car args)))
	  ((eq? procedure '+) (apply-arithmetic + args 0))
	  ((eq? procedure '-) (apply-arithmetic - (cdr args) (car args)))
	  ((eq? procedure '*) (apply-arithmetic * args 1))
	  ((eq? procedure '/) (apply-arithmetic / (cdr args) (car args)))
	  ((eq? procedure 'list) (make-list args))
	  ((eq? procedure 'eq?) (eq? (car args) (cadr args)))
	  ((eq? procedure '=) (eq? (car args) (cadr args)))
	  (else
	    (display "not implemented yet")))))

(define (apply-arithmetic proc args accum)
  (if (null? args)
    accum
    (apply-arithmetic proc (cdr args) (proc accum (car args)))))

(define (make-list args)
  (if (null? args)
    '()
    (cons (car args) (make-list (cdr args)))))

(define (apply-compound-procedure proc arguments)
  (let ((procedure (lookup-var proc global-environment)))
    (let ((result 
	    (meval-sequence 
	      (caddr procedure)
	      (extend-environment 
		(cadr procedure)
		arguments
		(car global-environment)))))
      result)))


;;; Creating a procedure to be run
(define (make-procedure params body env)
  (list 'procedure params body))

;;; //////////////////////////
;;; Interpreter Loop
;;; //////////////////////////

;;; Driver for REPL, just calls recursive driver with initial environment.
(define (driver)
  (recDriver global-environment))

(define (recDriver env)
  (display "meta> ")
  (let ((in (read)))
        (let ((out (meval in env)))
	  (display "\n")
	  (display ";Value: ")
	  (display out)
	  (display "\n\n")
          (recDriver env))))
