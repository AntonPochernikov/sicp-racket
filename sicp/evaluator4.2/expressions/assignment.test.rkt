#lang racket

(require sicp)
(require rackunit)
(require "assignment.rkt")

(define test '(set! x 5))

(check-true (assignment? test))
(check-equal? (assignment-variable test) 'x)
(check-eq? (assignment-value test) 5)
