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

(define ones (cons-stream 1 ones))
(define integers
  (cons-stream 1 (add-streams ones integers)))

(define (display-stream s n)
  (cond [(stream-null? s) 'done]
        [(= n 0) 'done]
        [else
         (display (stream-car s))
         (newline)
         (display-stream (stream-cdr s)
                         (- n 1))]))

; SOLUTION
(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream
       (stream-car s1)
       (interleave s2 (stream-cdr s1)))))

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))

; the pair with one as car will be every second pair of the stream
; assuming that next j will be previous plus one we can say that
; (1 100) will be 200th pair
; actually we will get it on 198th position
; cause first 2 elements will be pairs of 1 i
(stream-ref (pairs integers integers) 197)

; we will get next n element of i on eveery 2^n - 1 posision
; so every next i will have to wait double amount of time
; according to previous one
(display-stream (pairs integers integers) 16)
; 2^2 - 1 = 3 => (2 2)
; 2^3 - 1 = 7 => (3 3)
; 2^4 - 1 = 15 => (4 4)

; (99 100) is on 2^99 + 2^98 - 1 position
; (100 100) is on 2^100 - 1 position

















