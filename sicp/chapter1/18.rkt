#lang sicp

(#%require rackunit)

(define (double x) (+ x x))
(define (half x) (/ x 2))
(define (-- x) (- x 1))

(define (fast-expt a b)
  (define (iter acc rest prod)
    (cond ((= rest 0) acc)
          ((even? rest) (iter acc (half rest) (double prod)))
          (else (iter ((if (> b 0) + 0) acc prod)
                      (-- rest)
                      prod))))
    (if (or (= a 0) (= b 0))
        0
        (iter 0 (abs b) a)))

(check-equal? (fast-expt 1 5) 5)
(check-equal? (fast-expt 5 1) 5)
(check-equal? (fast-expt -3 3) -9)
(check-equal? (fast-expt 2 6) 12)
