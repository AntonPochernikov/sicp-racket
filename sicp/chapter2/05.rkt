#lang sicp

(#%require rackunit)

(define (cons a b)
  (* (expt 2 a) (expt 3 b)))

(define (car pair)
  (define (iter acc rest)
    (if (= (remainder rest 2) 0)
        (iter (inc acc) (/ rest 2))
        acc))
  (iter 0 pair))

(define (cdr pair)
  (define (iter acc rest)
    (if (= (remainder rest 3) 0)
        (iter (inc acc) (/ rest 3))
        acc))
  (iter 0 pair))

(define pair-test (cons 1 2))
(check-equal? (car pair-test) 1)
(check-equal? (cdr pair-test) 2)
