#lang sicp

(#%require rackunit)

(define (compose f g) (lambda (x) (f (g x))))

(define (square x) (* x x))

(check-equal? ((compose square inc) 6) 49)
