#lang racket

(require sicp)
(require rackunit)
(require "begin.rkt")

(define test (make-begin '(make something '())))

(check-equal? test '(begin make something '()))

(check-true (begin? test))
(check-false (begin? '()))

(check-equal? (begin-actions test) '(make something '()))

(check-equal? (sequence->exp (list 1 2 3 4 5))
              '(begin 1 2 3 4 5))




