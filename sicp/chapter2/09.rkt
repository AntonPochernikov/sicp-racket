#lang sicp

(#%require rackunit)

(define (make-interval a b) (cons a b))
(define (lower-bound interval) (car interval))
(define (upper-bound interval) (cdr interval))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (lower-bound y))
                 (- (upper-bound x) (upper-bound y))))

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

(define (interval-raduis interval)
  (/ (- (upper-bound interval)
        (lower-bound interval))
     2))
(define interval-1 (make-interval 2.5 5.0))
(define interval-2 (make-interval 1.0 3.0))

(check-equal? (interval-raduis (add-interval interval-1 interval-2))
              (+ (interval-raduis interval-1) (interval-raduis interval-2)))

(check-equal? (interval-raduis (sub-interval interval-1 interval-2))
              (- (interval-raduis interval-1) (interval-raduis interval-2)))

(check-equal? ( = (interval-raduis (mul-interval interval-1 interval-2))
                  (* (interval-raduis interval-1) (interval-raduis interval-2))) #f)

(check-equal? ( = (interval-raduis (div-interval interval-1 interval-2))
                  (/ (interval-raduis interval-1) (interval-raduis interval-2))) #f)
