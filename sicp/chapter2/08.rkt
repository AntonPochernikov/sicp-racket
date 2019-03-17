#lang sicp

(#%require rackunit)

(define (make-interval a b) (cons a b))
(define (lower-bound interval) (car interval))
(define (upper-bound interval) (cdr interval))
(define (print-interval interval)
  (display "[")
  (display (car interval))
  (display " - ")
  (display (cdr interval))
  (display "]")
  (newline))

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (lower-bound y))
                 (- (upper-bound x) (upper-bound y))))

(define interval-1 (make-interval 2.5 5.0))
(define interval-2 (make-interval 1.0 3.0))
(define interval-3 (sub-interval interval-1 interval-2))
(check-equal? (lower-bound interval-3) 1.5)
(check-equal? (upper-bound interval-3) 2.0)

