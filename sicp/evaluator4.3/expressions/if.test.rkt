#lang racket

(require rackunit)
(require "if.rkt")

(define if-list (make-if '(> x 5) 'false 'true))

(check-equal?
 if-list
 '(if (> x 5) false true))

(check-true (if? if-list))
(check-false (if? '(iff)))

(check-equal? (if-predicate if-list) '(> x 5))
(check-eq? (if-consequent if-list) 'false)
(check-eq? (if-alternative if-list) 'true)







