#lang racket

(require sicp)
(require rackunit)
(require "environment.rkt")

(define test (extend-environment
              '(x)
              (list 5)
              (extend-environment '(x y z)
                                  (list 1 3 6)
                                  the-empty-environment)))

(check-equal? (lookup-variable-value 'x test) 5)
(check-equal? (lookup-variable-value 'y test) 3)
(check-equal? (lookup-variable-value 'z test) 6)

(define-variable! 'y 4 test)

(check-equal? (lookup-variable-value 'y test) 4)

(set-variable-value! 'z 8 test)

(check-equal? (lookup-variable-value 'z test) 8)

(unbound-variable! 'x test)

(check-equal? (lookup-variable-value 'x test) 1)






