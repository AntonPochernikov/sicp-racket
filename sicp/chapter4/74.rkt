#lang racket

(require sicp)

; a)
(define (simple-stream-flatmap proc s)
  (simple-flatten (stream-map proc s)))

(define (simple-flatten stream)
  (stream-map stream-car
              (stream-filter
               (lambda (s)
                 (not (stream-null? s)))
               stream)))

; b)
; I don't think there is a difference.











