#lang sicp

(#%require rackunit)

(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

(define (identity x) x)
(define (inc x) (+ x 1))
(define (square x) (* x x))


(check-equal? (sum identity 1 inc 5) 15)
(check-equal? (sum square 4 inc 5) 41)
