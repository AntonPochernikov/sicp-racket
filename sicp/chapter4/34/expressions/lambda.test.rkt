#lang racket

(require sicp)
(require rackunit)
(require "lambda.rkt")

(define test '(lambda (x) (* x x)))

(check-true (lambda? test))
(check-false (lambda? '()))
(check-equal? (lambda-parameters test) '(x))
(check-equal? (lambda-body test) '((* x x)))

(define test2 '(lambda (a (b lazy) c (d lazy-memo)) 5))

(check-equal? (lambda-parameters test2) '(a (b lazy) c (d lazy-memo)))
(check-equal? (lambda-body test2) '(5))







