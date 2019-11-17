#lang racket

(require sicp)
(require rackunit)
(require "lambda.rkt")

(define test '(lambda (x) (* x x)))

(check-true (lambda? test))
(check-false (lambda? '()))
(check-equal? (lambda-parameters test) '(x))
(check-equal? (lambda-body test) '((* x x)))





