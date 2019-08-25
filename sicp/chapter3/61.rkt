#lang racket

(require sicp)
(require rackunit)

(define (square x) (* x x))

(define (stream-car s) (car s))
(define (stream-cdr s) (force (cdr s)))

(define (stream-ref s n)
  (cond [(= n 0) (stream-car s)]
        [(stream-null? s) (error "No such ref in the stream: STREAM-REF" s n)]
        [else
         (stream-ref (stream-cdr s)
                     (- n 1))]))

(define (some pred L)
  (cond [(null? L) false]
        [(pred (car L)) true]
        [else (some pred (cdr L))]))

(define (stream-map proc . argstreams)
  (if (some stream-null? argstreams)
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))

(define (scale-stream s factor)
  (stream-map (lambda (x) (* factor x)) s))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (mul-streams s1 s2)
  (stream-map * s1 s2))

(define ones (cons-stream 1 ones))
(define integers
  (cons-stream 1 (add-streams integers ones)))

(define (integrate-series s)
  (stream-map / s integers))

(define cosine-stream
  (cons-stream 1
               (integrate-series sine-stream)))
(define sine-stream
  (cons-stream 0
               (integrate-series
                (stream-map - cosine-stream))))

(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1)
                  (stream-car s2))
               (add-streams (scale-stream (stream-cdr s2) (stream-car s1))
                            (mul-series (stream-cdr s1) s2))))

; SOLUTION
(define (invert-unit-series s)
  (cons-stream
   1
   (scale-stream (mul-series (stream-cdr s)
                             (invert-unit-series s))
                 -1)))

(define (print-n s n)
  (cond [(stream-null? s) the-empty-stream]
        [(= n 0) #f]
        [(= n 1)
         (begin (display (stream-car s))
                (newline))]
        [else (begin (display (stream-car s))
                     (newline)
                     (print-n (stream-cdr s) (- n 1)))]))

(print-n (mul-series (invert-unit-series integers) integers) 5)









