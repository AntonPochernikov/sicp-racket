#lang sicp

(#%require rackunit)

(define (make-interval a b) (cons a b))
(define (lower-bound i) (car i))
(define (upper-bound i) (cdr i))
(define (print-interval i)
  (display "[")
  (display (car i))
  (display " - ")
  (display (cdr i))
  (display "]")
  (newline))

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (make-center-percent c wp) (make-center-width c (/ wp 100)))

(define (percent i) (* (width i) 100))

(define interval-1 (make-center-percent 3.5 15))
(print-interval interval-1)
(display (center interval-1))
(newline)
(display (width interval-1))
(newline)
(display (percent interval-1))
