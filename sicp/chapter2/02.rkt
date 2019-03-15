#lang sicp

(#%require rackunit)

(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))
(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ", ")
  (display (y-point p))
  (display ")"))

(define (make-segment p1 p2) (cons p1 p2))
(define (start-segment s) (car s))
(define (end-segment s) (cdr s))

(define (midpoint-segment s)
  (let ((start (start-segment s))
        (end (end-segment s))
        (average (lambda (a b) (/ (+ a b) 2))))
    (make-point (average (x-point start) (x-point end))
                (average (y-point start) (y-point end)))))

(define segment (make-segment (make-point -2 6)
                              (make-point 2 4)))

(check-equal? (x-point (midpoint-segment segment)) 0)
(check-equal? (y-point (midpoint-segment segment)) 5)
(print-point (midpoint-segment segment))
