#lang racket/base

(require rackunit)

(define (square x) (* x x))
(define (cube x) (* (square x) x))
(define (abs x)
  (if (< x 0)
       (- x)
       x))

(define (good-enough? prev next)
  (< (abs (- next prev)) 0.001))

(define (improve guess x)
  (/
   (+
    (/ x (square guess))
    (* 2 guess))
   3))

(define (cubic-root-iter guess x)
  (if (good-enough? guess (improve guess x))
      guess
      (cubic-root-iter (improve guess x) x)))

(define (cubic-root x) (cubic-root-iter 1.0 x))

(check-equal? (cubic-root 1000) 10.000000145265767)
(check-equal? (cubic-root 40) 3.4200720972330565)
