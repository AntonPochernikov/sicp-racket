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

(define ones (cons-stream 1 ones))
(define integers
  (cons-stream 1
               (add-streams integers ones)))

; SOLUTION
(define partial-sums
  (cons-stream 0 (add-streams integers partial-sums)))

(check-equal? (stream-ref partial-sums 1) 1)
(check-equal? (stream-ref partial-sums 2) 3)
(check-equal? (stream-ref partial-sums 3) 6)
(check-equal? (stream-ref partial-sums 4) 10)
(check-equal? (stream-ref partial-sums 5) 15)















