#lang sicp

(#%require rackunit)

(define (double f)
  (lambda (x) (f (f x))))

(check-equal? (((double (double double)) inc) 5) 21)