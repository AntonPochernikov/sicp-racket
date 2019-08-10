#lang racket

(require sicp)

(define (stream-car s) (car s))
(define (stream-cdr s) (force (cdr s)))

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))
