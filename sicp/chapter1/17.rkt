#lang sicp

(#%require rackunit)
(define (double x) (+ x x))
(define (half x) (/ x 2))
(define (-- x) (- x 1))

(define (fast-expt a b)
  (cond ((or (= a 0) (= b 0)) 0)
        ((< b 0) (fast-expt (- a) (- b)))
        ((even? b ) (double (fast-expt a (half b))))
        (else (+ a (fast-expt a (-- b))))))

(check-equal? (fast-expt 1 5) 5)
(check-equal? (fast-expt 5 1) 5)
(check-equal? (fast-expt -3 3) -9)
(check-equal? (fast-expt 2 6) 12)
