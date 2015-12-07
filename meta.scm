;;; Environment stuff
;;; Relies on environment functions implemented in mit-scheme

(define global-environment (list user-initial-environment))
;;; One of two main entry points to metacircular evaluator. Evaluates
;;; expressions. todo: learn more about this
(define (meval expr envs)
  (cond ((self-evaluating? expr) expr)
	((variable? expr) (lookup-var envs expr))
        ((quote? expr) (cadr expr))

        ((lambda? expr) (display "\n;Value: Is lambda\n"))
        ((if? expr) (display "\n;Value: Is if\n"))
        ((define? expr) (meval-define (car (cdr expr)) (car (cdr (cdr expr))) envs))
        ((cond? expr) (display "\n;Value: Is cond\n"))
	((apply? expr) (mapply (car expr) (cdr expr)))
        (else (error "eval failed/not implemented\n"))))

;;; Other main entry point into metacircular evaluator. Applys arguments
;;; to a procedures.
(define (mapply procedure arguments)
  (cond ((primitive-procedure? procedure)
	 (apply-primitive-procedure procedure arguments))
	((compound-procedure? procedure)
	 (apply-compount-procedure procedure arguments))
	(else
	  (error "wat"))))

;;; Eval's condition checks

;;; Determines if the expression is already in a form that can be evaluated.
;;; i.e. single numerics, strings, etc.
(define (self-evaluating? expr)
  (cond ((number? expr) #t)
        ((string? expr) #t)
        ((boolean? expr) #t)
        (else (char? expr))))

;;; Helper methods to check if expressions start with specific symbols
(define (variable? expr)
  (symbol? expr))

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

(define (apply? expr)
  (pair? expr))

;;; Checks if the given expression begins with specified quoted symbol,
;;; i.e. begins with ', (starts-with-symbol? '(+ 1 2) '+) == #t
(define (starts-with-symbol? expr symbol)
  (if (pair? expr)
    (eq? (car expr) symbol)
    #f))

;; Eval code

;; TODO
(define (lookup-var envs symbol)
  (define (lookup-recurse envs)
    (if (null? envs)
      (error "baka")
      (let ((result (environment-bound? (car envs) symbol)))
	(if (eq? result #f)
	  (lookup-recurse (cdr envs))
	  (environment-lookup (car envs) symbol)))))
  (lookup-recurse envs))

;;; TODO
(define (meval-define symbol expr envs)
  (environment-define (car envs) symbol (meval expr envs)))


;;; Primitive procedure table, we will look up primitives to get the compiled procedures here
(define primitive-table
  (list (list 'car car)
        (list 'cdr cdr)
        ))

;;; Apply checks

(define (primitive-procedure? proc)
  (cond ((eq? proc 'car) #t)
        ((eq? proc 'cdr) #t)
	(else #f)))


;;; Apply Code

(define (retrieve-procedure proc table)
  (if (null? (car table))
    (error "Procedure not found"))
  (display (car (car table)))
  (if (eq? proc (car (car table)))
    (car (cdr (car table)))
    (retrieve-procedure proc (cdr table))))

(define (apply-primitive-procedure proc args)
  (let ((procedure (retrieve-procedure proc primitive-table))
        (evaluated-args (meval (car args) global-environment)))
    (procedure evaluated-args)))

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
