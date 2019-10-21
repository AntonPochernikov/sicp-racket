#lang racket

(require rackunit)

; a
(define fib
  (lambda (n)
    ((lambda (fibs) (fibs fibs n))
     (lambda (f x) (if (< x 3)
                       1
                       (+ (f f (- x 1))
                          (f f (- x 2))))))))

(check-eq? (fib 5) 5)
(check-eq? (fib 1) 1)
(check-eq? (fib 2) 1)
  

; b
(define (f x)
  ((lambda (even? odd?) (even? even? odd? x))
   (lambda (ev? od? n)
     (if (= n 0) true (od? ev? od? (- n 1))))
   (lambda (ev? od? n)
     (if (= n 0) false (ev? ev? od? (- n 1))))))

(check-true (f 4))
(check-false (f 7))
(check-true (f 0))



