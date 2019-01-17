#lang racket/base

(require rackunit)

(define (square x) (* x x))
(define (abs x)
  (if (< x 0)
       (- x)
       x))

(define (good-enough? prev next)
  (< (abs (- next prev)) 0.001))

(define (average x y) (/ (+ x y) 2))
(define (improve guess x) (average guess (/ x guess)))

(define (sqrt-iter guess x)
  (if (good-enough? guess(improve guess x))
      guess
      (sqrt-iter (improve guess x) x)))

(define (sqrt x)
  (sqrt-iter 1.0 x))

(check-equal? (sqrt 9) 3.00009155413138)
(check-equal? (sqrt (+ 100 37)) 11.705105833379696)
(check-equal? (square (sqrt 1000)) 1000.000369924366)