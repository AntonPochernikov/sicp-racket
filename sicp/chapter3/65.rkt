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

(define (display-stream s n)
  (cond [(stream-null? s) 'done]
        [(= n 0) 'done]
        [else
         (display (stream-car s))
         (newline)
         (display-stream (stream-cdr s)
                         (- n 1))]))

(define (partial-sums s)
  (cons-stream
   (stream-car s)
   (stream-map (lambda (x) (+ x (stream-car s)))
               (partial-sums (stream-cdr s)))))

; SOLUTION
(define (euler-transform s)
  (let ([s0 (stream-ref s 0)]
        [s1 (stream-ref s 1)]
        [s2 (stream-ref s 2)])
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))

(define (make-tableau transform s)
  (cons-stream s (make-tableau transform (transform s))))

(define (accelerated-sequence transform s)
  (stream-map stream-car (make-tableau transform s)))

(define (ln2-sums n)
  (cons-stream (/ 1.0 n)
               (stream-map - (ln2-sums (+ n 1)))))

(define ln2-stream
  (partial-sums (ln2-sums 1)))

(display-stream ln2-stream 8)
(newline)
(display-stream (euler-transform ln2-stream) 8)
(newline)
(display-stream (accelerated-sequence euler-transform ln2-stream) 8)


















