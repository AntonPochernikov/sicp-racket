#lang racket

(require sicp)

; Procedure won't work cause we are referencing unassigned variable y
; in inner let block.
; (delay dy) is transformed to (lambda () dy).
; Defining b must compute first value in stream that is
; returned by (stream-map f y). But y is unassigned at that moment.
(define solve
  (lambda (f y0 dt)
    (let ((y '*unassigned*) (dy '*unassigned*))
      (let ((a (integral (delay dy) y0 dt))
            (b (stream-map f y)))
        (set! y a)
        (set! dy b))
      y)))

; This way we will reference y that is already assigned
; This procedure will work just fine
(define solve2
  (lambda (f y0 dt)
    (let ((y '*unassigned*) (dy '*unassigned*))
      (set! y (integral (delay dy) y0 dt))
      (set! dy (stream-map f y))
      y)))







