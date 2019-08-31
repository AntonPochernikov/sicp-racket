#lang racket

(require sicp)
(require rackunit)

(define (square x) (* x x))
(define (average x y) (/ (+ x y) 2))

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

; SOLUTION
(define (sqrt-stream x)
  (define (sqrt-improve guess)
    (average guess (/ x guess)))
  (define guesses
    (cons-stream 1.0
                 (stream-map sqrt-improve
                             guesses)))
  guesses)

(define (stream-limit stream tolerance)
  (define (good-enough? a b)
    (< (abs (- a b)) tolerance))
  (define (iter prev current s)
    (if (good-enough? prev current)
        current
        (iter current
              (stream-car s)
              (stream-cdr s))))
  (iter (stream-ref stream 0)
        (stream-ref stream 1)
        stream))

(define (sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))

(sqrt 16 .00001)












