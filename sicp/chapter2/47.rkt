#lang sicp

(#%require rackunit)

(define (make-vect x y) (cons x y))
(define (xcor-vect vect) (car vect))
(define (ycor-vect vect) (cdr vect))
(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1) (xcor-vect v2)) (+ (ycor-vect v1) (ycor-vect v2))))
(define (sub-vect v1 v2)
  (make-vect (- (xcor-vect v1) (xcor-vect v2)) (- (ycor-vect v1) (ycor-vect v2))))
(define (scale-vect vect s)
  (make-vect (* (xcor-vect vect) s) (* (ycor-vect vect) s)))

(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame frame) (car frame))

(define (edge1-frame frame) (cadr frame))

(define (edge2-frame frame) (caddr frame))


(define f (make-frame (make-vect 0 0) (make-vect 1 0) (make-vect 0 1)))
(check-equal? (origin-frame f) (make-vect 0 0))
(check-equal? (edge1-frame f) (make-vect 1 0))
(check-equal? (edge2-frame f) (make-vect 0 1))
