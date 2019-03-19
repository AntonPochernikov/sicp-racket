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

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))
(define (make-center-percent c p)
   (make-center-width c (* c (/ p 100.0))))

(define (percent i) (* (/ (width i) (center i)) 100.0))

(define interval-1 (make-center-percent 10.0 5))
(define interval-2 (make-center-percent 3.0 5))

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

(print-interval (par1 interval-1 interval-1))
(print-interval (par2 interval-1 interval-1))
(print-interval (par1 interval-1 interval-2))
(print-interval (par2 interval-1 interval-2))
(newline)
(display (center (div-interval interval-1 interval-1)))
(newline)
(display "center of dividing interval by itself should be 1 but its not")
