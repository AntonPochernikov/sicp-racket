#lang sicp

(define (square x) (* x x))
(define (cube x) (* x x x))
(define dx 0.00001)

(define (cubic a b c)
  (lambda (x) (+ (cube x) (* a (square x)) (* b x) c)))

