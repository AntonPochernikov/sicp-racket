#lang sicp

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (average a b) (/ (+ a b) 2))
(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (compose f g) (lambda (x) (f (g x))))
(define (repeated f times)
  (if (<= times 1)
      f
      (compose f (repeated f (- times 1)))))

(define (square x) (* x x))
(define (-- x) (- x 1))

(define (fast-expt x n)
  (define (iter acc rest prod)
    (cond ((= rest 0) acc)
          ((even? rest) (iter acc (/ rest 2) (square prod)))
          (else (iter (* acc prod) (-- rest) prod))))
  (if (= n 0) 1 (iter 1 n x)))

(define (nth-root x times)
  (fixed-point ((repeated average-damp (- times 2)) (lambda (y)
                               (/ x (fast-expt y (- times 1)))))
               1.0))

(nth-root 8 3)
(nth-root 81 4)
(nth-root 100 2)
