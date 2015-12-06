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
        (else "Eval failed/Not implemented")))

;;; Driver for REPL, just calls recursive driver with initial environment.
(define (driver)
  (recDriver user-initial-environment))

(define (recDriver env)
  (let ((expr (read))
        (newEnv (meval expr env)))
    (recDriver newEnv)))
