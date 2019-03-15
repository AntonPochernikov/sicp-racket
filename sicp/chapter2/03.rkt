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

(define (square x) (* x x))
(define (length-segment s)
   (let ((start (start-segment s))
        (end (end-segment s)))
     (sqrt (+ (square (- (x-point end) (x-point start)))
              (square (- (y-point end) (y-point start)))))))
(define segment-2 (make-segment (make-point 0 0)
                              (make-point 3 4)))

(define (make-rect top right) (cons top right))
(define (top-rect rect) (car rect))
(define (right-rect rect) (cdr rect))

(define (square-rect rect)
  (* (length-segment (top-rect rect))
     (length-segment (right-rect rect))))

(define (perimeter-rect rect)
  (+ (* 2 (length-segment (top-rect rect)))
     (* 2 (length-segment (right-rect rect)))))

(define rect-1 (make-rect (make-segment (make-point 0 2)
                                        (make-point 2 2))
                          (make-segment (make-point 2 2)
                                        (make-point 2 0))))

(check-equal? (square-rect rect-1) 4)
(check-equal? (perimeter-rect rect-1) 8)

(define (make-rect-2 top-left top-right bottom-right) (cons (cons top-left top-right) bottom-right))
(define (top-rect-2 rect) (make-segment (car (car rect)) (cdr (car rect))))
(define (right-rect-2 rect) (make-segment (cdr (car rect)) (cdr rect)))

(define (square-rect-2 rect)
  (* (length-segment (top-rect-2 rect))
     (length-segment (right-rect-2 rect))))

(define (perimeter-rect-2 rect)
  (+ (* 2 (length-segment (top-rect-2 rect)))
     (* 2 (length-segment (right-rect-2 rect)))))

(define rect-2 (make-rect-2 (make-point 0 2) (make-point 2 2) (make-point 2 0)))

(check-equal? (square-rect-2 rect-2) 4)
(check-equal? (perimeter-rect-2 rect-2) 8)
