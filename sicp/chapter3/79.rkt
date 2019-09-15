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

(define (solve f y0 dt)
  (let ([y 'empty] [dy 'empty])
    (set! y (integral (delay dy) y0 dt))
    (set! dy (stream-map f y))
    y))


(define (solve-2nd A B dt y0 dy0)
  (let ([dy 'empty] [ddy 'empty] [y 'empty])
    (set! y (integral (delay dy) y0 dt))
    (set! dy (integral (delay ddy) dy0 dt))
    (set! ddy (add-streams (scale-stream dy A)
                           (scale-stream y B)))
    y))


; SOLUTION
(define (general-solve-2nd f y0 dy0 dt)
  (let ([dy 'empty] [ddy 'empty] [y 'empty])
    (set! y (integral (delay dy) y0 dt))
    (set! dy (integral (delay ddy) dy0 dt))
    (set! ddy (stream-map f dy y))
    y))

(stream-ref (general-solve-2nd (lambda (dy y) dy) 1 1 0.0001) 50)

















