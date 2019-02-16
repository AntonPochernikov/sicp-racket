#lang sicp

(#%require rackunit)

(define psi (/ (- 1 (sqrt 5)) 2))
(define phi (/ (+ 1 (sqrt 5)) 2))

(define (^ n exp)
  (define (iter acc counter)
    (if (= counter 0)
         acc
         (iter (* acc n) (- counter 1))))
  (iter 1 exp))

(define (fib n)
   (/ (^ phi n) (sqrt 5)))

(check-equal? (round (fib 3)) 2.0)
(check-equal? (round (fib 5)) 5.0)
(check-equal? (round (fib 7)) 13.0)
