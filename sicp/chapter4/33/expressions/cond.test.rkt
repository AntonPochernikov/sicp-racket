#lang racket

(require sicp)
(require rackunit)
(require "cond.rkt")

(define test
  '(cond ((= x 5) 5)
         (else 25)))

(check-true (cond? test))
(check-false (cond? '()))

(check-equal? (cond->if test)
              '(if (= x 5)
                   5
                   25))

(define test2
  '(cond ((= x 5) 25)
         ((= x 3) x)
         (else 5)))
(check-equal? (cond->if test2)
              '(if (= x 5)
                   25
                   (if (= x 3)
                       x
                       5)))






