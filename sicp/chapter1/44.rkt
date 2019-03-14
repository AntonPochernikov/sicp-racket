#lang sicp

(define dx 0.00001)
(define (smooth f)
  (define (average a b c) (/ (+ a b c) 3))
  (lambda (x)
    (average (f (- x dx))
             (f x)
             (f (+ x dx)))))

(define (<= a b) (or (< a b) (= a b)))
(define (compose f g) (lambda (x) (f (g x))))
(define (repeated f times)
  (if (<= times 1)
      f
      (compose f (repeated f (- times 1)))))

(define (n-fold-smoothed f n)
  ((repeated smooth n) f))
