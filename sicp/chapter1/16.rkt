#lang sicp

(#%require rackunit)

(define (square x) (* x x))
(define (-- x) (- x 1))

(define (fast-sqrt x n)
  (define (iter acc rest prod)
    (cond ((= rest 0) acc)
          ((even? rest) (iter acc (/ rest 2) (square prod)))
          (else (iter (* acc prod) (-- rest) prod))))
  (if (= n 0) 1 (iter 1 n x)))

(check-equal? (fast-sqrt 1 5) 1)
(check-equal? (fast-sqrt 5 1) 5)
(check-equal? (fast-sqrt -3 3) -27)
(check-equal? (fast-sqrt 2 6) 64)
