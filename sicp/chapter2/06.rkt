#lang sicp

(#%require rackunit)

(define zero (lambda (f) (lambda (x) x)))
(define (add-1 n)
  (lambda (f)
    (lambda (x) (f((n f) x)))))

(check-equal? ((zero inc) 1) 1)

(define one (lambda (f)
              (lambda (x) (f x))))
(check-equal? ((one inc) 1) 2)

(define two (lambda (f)
              (lambda (x) (f (f x)))))
(check-equal? ((two inc) 1) 3)

(define (add n1 n2)
  (lambda (f)
    (lambda (x)
      ((n1 f) ((n2 f) x)))))

(define three (add one two))
(check-equal? ((three inc) 2) 5)

(define five (add two three))

(check-equal? ((five inc) 2) 7)

(define ten (add five five))
(check-equal? ((ten inc) 2) 12)
