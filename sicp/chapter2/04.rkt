#lang sicp

(#%require rackunit)

(define (cons x y)
  (lambda (selector) (selector x y)))

(define (car pair)
  (pair (lambda (x y) x)))

(define (cdr pair)
  (pair (lambda (x y) y)))

(define pair-test (cons 1 2))
(check-equal? (car pair-test) 1)
(check-equal? (cdr pair-test) 2)
