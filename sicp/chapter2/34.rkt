#lang sicp

(#%require rackunit)

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coef higher-terms)
                (+ this-coef (* x higher-terms)))
              0
              coefficient-sequence))

(check-equal? (horner-eval 2 '(1 3 0 5 0 1)) 79)
(check-equal? (horner-eval 3 '(0 2 2 2 1)) 159)
