#lang sicp

(#%require rackunit)

(define (make-accumulator acc)
  (lambda (amount)
    (begin (set! acc (+ acc amount))
           acc)))

(define A (make-accumulator 5))
(check-equal? (A 10) 15)
(check-equal? (A 10) 25)


(define A2 (make-accumulator 20))
(check-equal? (A2 -20) 0)
