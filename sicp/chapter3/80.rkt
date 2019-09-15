#lang racket

(require sicp)
(require rackunit)

(define (square x) (* x x))
(define (cube x) (* x x x))
(define (average a b) (/ (+ a b) 2))
(define (divisible? a b)
  (= (remainder a b) 0))
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

(define (stream-filter predicate s)
  (cond [(stream-null? s) the-empty-stream]
        [(predicate (stream-car s))
         (cons-stream (stream-car s)
                      (stream-filter predicate (stream-cdr s)))]
        [else
         (stream-filter predicate (stream-cdr s))]))

(define (scale-stream s factor)
  (stream-map (lambda (x) (* factor x)) s))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define ones (cons-stream 1 ones))
(define integers
  (cons-stream 1 (add-streams ones integers)))

(define (display-stream s n)
  (cond [(stream-null? s) 'done]
        [(= n 1)
         (display (stream-car s))
         (newline)
         'done]
        [else
         (display (stream-car s))
         (newline)
         (display-stream (stream-cdr s)
                         (- n 1))]))

(define (integral delayed-integrand initial-value dt)
  (cons-stream initial-value
               (let ([integrand (force delayed-integrand)])
                 (if (stream-null? integrand)
                     the-empty-stream
                     (integral (delay (stream-cdr integrand))
                               (+ (* dt (stream-car integrand))
                                  initial-value)
                               dt)))))

; SOLUTION
(define (RLC R L C dt)
  (let ([dvC 'empty]
        [diL 'empty]
        [vC 'empty]
        [iL 'empty])
    (lambda (vC0 iL0)
      (set! vC (integral (delay dvC) vC0 dt))
      (set! iL (integral (delay diL) iL0 dt))
      (set! dvC (scale-stream iL (- (/ 1 C))))
      (set! diL (add-streams (scale-stream iL (- (/ R L)))
                             (scale-stream vC (/ 1 L))))
      (cons vC iL))))

(define test ((RLC 1 0.2 1 0.1) 0 10))

(display-stream (car test) 10)
(display-stream (cdr test) 10)















