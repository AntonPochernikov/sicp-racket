#lang sicp

(#%require rackunit)

(define (make-interval a b) (cons a b))
(define (lower-bound interval) (car interval))
(define (upper-bound interval) (cdr interval))

(define (mul-interval i1 i2)
  (let ((p1 (* (lower-bound i1) (lower-bound i2)))
        (p2 (* (lower-bound i1) (upper-bound i2)))
        (p3 (* (upper-bound i1) (lower-bound i2)))
        (p4 (* (upper-bound i1) (upper-bound i2))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval i1 i2)
  (define div-zero? (not (or (and (> (lower-bound i2) 0)
                                    (> (upper-bound i2) 0))
                               (and (< (lower-bound i2) 0)
                                    (< (upper-bound i2) 0)))))
  (if div-zero?
      (error "Can not divide to zero-crossing interval")
      (mul-interval i1
                (make-interval (/ 1 (upper-bound i2))
                               (/ 1 (lower-bound i2))))))

(define interval-1 (make-interval 2.5 5.0))
(define interval-2 (make-interval -1.0 5.0))

(div-interval interval-1 interval-2)