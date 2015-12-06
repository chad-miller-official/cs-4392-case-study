;;; Checks if the given expression begins with specified quoted symbol,
;;; i.e. begins with ', (starts-with-symbol? '(+ 1 2) '+) == #t
(define (starts-with-symbol? expr symbol)
  (if (pair? expr)
    (eq? (car expr) symbol)
    #f))

;;; Helper methods to check if expressions start with specific symbols
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

;;; Determines if the expression is already in a form that can be evaluated.
;;; i.e. single numerics, strings, etc.
(define (self-evaluating? expr)
  (cond ((number? expr) #t)
        ((string? expr) #t)
        ((boolean? expr) #t)
        ((vector? expr) #t)
        (else (char? expr))))

;;; One of two main entry points to metacircular evaluator. Evaluates
;;; expressions. todo: learn more about this
(define (meval expr env)
  (cond ((self-evaluating? expr) expr)
        ((quote? expr) (display "\n;Value: Is quote\n"))
        ((lambda? expr) (display "\n;Value: Is lambda\n"))
        ((if? expr) (display "\n;Value: Is if\n"))
        ((define? expr) (display "\n;Value: Is define\n"))
        ((cond? expr) (display "\n;Value: Is cond\n"))
        (else (error "eval failed/not implemented\n"))))

;;; Driver for REPL, just calls recursive driver with initial environment.
(define (driver)
  (recDriver user-initial-environment))

(define (recDriver env)
  (display "meta> ")
  (let ((expr (read)))
        (let ((newEnv (meval expr env)))
	  (display "\n")
          (recDriver newEnv))))
