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

(define interval-1 (make-interval 2.5 5.0))
(check-equal? (lower-bound interval-1) 2.5)
(check-equal? (upper-bound interval-1) 5.0)

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval i1 i2)
  (let ((p1 (* (lower-bound i1) (lower-bound i2)))
        (p2 (* (lower-bound i1) (upper-bound i2)))
        (p3 (* (upper-bound i1) (lower-bound i2)))
        (p4 (* (upper-bound i1) (upper-bound i2))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval i1 i2)
  (mul-interval i1
                (make-interval (/ 1 (upper-bound i2))
                               (/ 1 (lower-bound i2)))))

(define interval-2 (make-interval 1.0 3.0))
(define interval-3 (add-interval interval-1 interval-2))
(check-equal? (lower-bound interval-3) 3.5)
(check-equal? (upper-bound interval-3) 8.0)

(define interval-5 (mul-interval interval-1 interval-2))
(check-equal? (lower-bound interval-5) 2.5)
(check-equal? (upper-bound interval-5) 15.0)

(define interval-6 (div-interval interval-5 interval-1))
(check-equal? (lower-bound interval-6) 0.5)
(check-equal? (upper-bound interval-6) 6.0)

