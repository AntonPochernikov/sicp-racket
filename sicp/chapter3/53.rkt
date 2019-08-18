#lang racket

(require sicp)
(require rackunit)

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

(define (add-streams s1 s2)
  (stream-map + s1 s2))

; SOLUTION

; resulting stream will equal to doubles
; 1 2 4 8 16 32 ...


(define s (cons-stream 1 (add-streams s s)))

(check-equal? (stream-ref s 1) 2)
(check-equal? (stream-ref s 5) 32)
(check-equal? (stream-ref s 10) 1024)








