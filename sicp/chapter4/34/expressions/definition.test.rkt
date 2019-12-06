#lang racket

(require sicp)
(require rackunit)
(require "definition.rkt")

(define test '(define x 5))

(check-true (definition? test))
(check-false (definition? '()))

(check-equal? (definition-variable test) 'x)
(check-eq? (definition-value test) 5)










