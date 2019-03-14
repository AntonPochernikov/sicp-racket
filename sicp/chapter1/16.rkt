#lang sicp

(#%require rackunit)

(define (square x) (* x x))
(define (-- x) (- x 1))

(define (fast-expt x n)
  (define (iter acc rest prod)
    (cond ((= rest 0) acc)
          ((even? rest) (iter acc (/ rest 2) (square prod)))
          (else (iter (* acc prod) (-- rest) prod))))
  (if (= n 0) 1 (iter 1 n x)))

(check-equal? (fast-expt 1 5) 1)
(check-equal? (fast-expt 5 1) 5)
(check-equal? (fast-expt -3 3) -27)
(check-equal? (fast-expt 2 6) 64)
