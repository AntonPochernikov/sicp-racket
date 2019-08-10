#lang racket

(require sicp)

(define (display-line x)
  (newline)
  (display x))

(define (stream-car s) (car s))
(define (stream-cdr s) (force (cdr s)))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream low
                   (stream-enumerate-interval (inc low) high))))

(define (stream-ref s n)
  (cond [(= n 0) (stream-car s)]
        [(stream-null? s) (error "No such ref in the stream: STREAM-REF" s n)]
        [else
         (stream-ref (stream-cdr s)
                     (- n 1))]))

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))

(define (show x)
  (display-line x)
  x)

(define x (stream-map show (stream-enumerate-interval 0 10)))

(stream-ref x 5)

(stream-ref x 7)

; first time we call stream-ref our stream has only its car evaled
; we eval 5 car's of the stream, now we don`t have to call show proc on these args
; cause we already have values memoized
; calling stream-ref with 7 will cause evaluations of (show 6) and (show 7)
















