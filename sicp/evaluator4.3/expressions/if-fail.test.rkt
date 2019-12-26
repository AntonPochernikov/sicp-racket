#lang racket

(require sicp)
(require rackunit)
(require "if-fail.rkt")

(define test (make-if-fail '(let (x 5) (even? x)) 'odd))

(check-equal? test '(if-fail (let (x 5) (even? x)) odd))

(check-true (if-fail? test))
(check-false (if-fail? '()))

(check-equal? (if-fail-consequent test)
              '(let (x 5) (even? x)))

(check-equal? (if-fail-alternative test)
              'odd)






