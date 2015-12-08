;;; Environment definition
;;; Relies on environment functions implemented in mit-scheme

(define global-environment (list user-initial-environment))

;;; One of two main entry points to metacircular evaluator. Evaluates
;;; expressions. 
(define (meval expr envs)
  (cond ((self-evaluating? expr) expr)
	((variable? expr) (lookup-var envs expr))
        ((quote? expr) (cadr expr))
        ((lambda? expr) (meval-lambda expr envs))
        ((if? expr) (meval-if (cdr expr) envs))
        ((define? expr) (meval-define 
			  (car (cdr expr)) 
			  (car (cdr (cdr expr))) 
			  envs))
        ((cond? expr) (display "\n;Value: Is cond\n"))
	((apply? expr) (mapply 
			 (car expr)
			 (list-of-eval-args (cdr expr) envs)))
        (else (error "eval failed/not implemented\n"))))


;;; Other main entry point into metacircular evaluator. Applys arguments
;;; to a procedures.
(define (mapply procedure arguments)
  (cond ((primitive-procedure? procedure)
	 (apply-primitive-procedure procedure arguments))
	((compound-procedure? procedure)
	 (display "compound")
	 (apply-compound-procedure procedure arguments))
	(else
	  (error "wat"))))

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

(define (lambda? expr)
  (starts-with-symbol? expr 'lambda))

(define (if? expr)
  (starts-with-symbol? expr 'if))

(define (define? expr)
  (starts-with-symbol? expr 'define))

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
(define (lookup-var envs symbol)
  (define (lookup-recurse envs)
    (if (null? envs)
      (begin 
	(display symbol)
        (display ": ")
        (error "Symbol does not exist"))
      (let ((result (environment-bound? (car envs) symbol)))
	(if (eq? result #f)
	  (lookup-recurse (cdr envs))
	  (environment-lookup (car envs) symbol)))))
  (lookup-recurse envs))

;;; Given an if statement and an environment, evaluates the if statement.
(define (meval-if expr envs)
  (let ((condition (car expr))
	(then-stmt (cadr expr))
	(else-stmt (caddr expr)))
    (if (meval condition envs)
      (meval then-stmt envs)
      (if (null? else-stmt) (meval else-stmt envs)))))

;;; Given a define statement and an environment,
;;; inserts variable definition into
(define (meval-define symbol expr envs)
  (environment-define (car envs) symbol (meval expr envs)))

;;; Given the arguments of a function, evaluates them to their most basic
;;; level and puts them in a list.
(define (list-of-eval-args exprs envs)
  (if (null? exprs)
    '()
    (cons (meval (car exprs) envs) (list-of-eval-args (cdr exprs) envs))))

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

;;; //////////////////////////
;;; Interpreter Loop
;;; //////////////////////////

;;; Driver for REPL, just calls recursive driver with initial environment.
(define (driver)
  (recDriver global-environment))

(define (recDriver envs)
  (display "meta> ")
  (let ((in (read)))
        (let ((out (meval in envs)))
	  (display "\n")
	  (display ";Value: ")
	  (display out)
	  (display "\n\n")
          (recDriver envs))))
