#lang sicp

(#%require rackunit)

(define (square x) (* x x))
(define (<= a b) (or (< a b) (= a b)))
(define (compose f g) (lambda (x) (f (g x))))

(define (repeated f times)
  (if (<= times 1)
      f
      (compose f (repeated f (- times 1)))))

(check-equal? ((repeated square 2) 5) 625)
(check-equal? ((repeated square 3) 2) 256)
