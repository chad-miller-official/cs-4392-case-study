;;; Checks if the given expression begins with specified quoted symbol,
;;; i.e. begins with ', (is-quoted? '(+ 1 2) '+) == #t
(define (is-symbol? expr symbol)
  (if (pair? expr) (eq? (car expr) symbol) #f))

;;; Determines if the expression is already in a form that can be evaluated.
;;; i.e. single numerics, strings, etc.
(define (self-evaluating? expr)
  (cond ((number? expr) #t)
        ((string? expr) #t)
        (else #f)))

;;; One of two main entry points to metacircular evaluator. Evaluates
;;; expressions. todo: learn more about this
(define (eval expr env)
  (if (self-evaluating? expr) expr))
