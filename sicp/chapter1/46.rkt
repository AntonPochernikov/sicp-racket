#lang sicp

(define (iterative-improve good-enough? improve)
  (lambda (guess x)
    (define (iter guess)
      (if (good-enough? guess)
        guess
        (iter (improve guess))))
    (iter guess)))


(define tolerance 0.00001)

(define (square x) (* x x))
(define (average a b) (/ (+ a b) 2))
(define (sqrt-iter guess x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) tolerance))
  (define (improve guess)
    (average guess (/ x guess)))
  ((iterative-improve good-enough? improve) guess x))

(sqrt-iter 2.0 9)


(define (fixed-point f first-guess)
  (define (good-enough? guess)
    (< (abs (- (f guess) guess)) tolerance))
  (define (improve guess) (f guess))
  ((iterative-improve good-enough? improve) (improve first-guess) first-guess))

(fixed-point cos 1.0)
