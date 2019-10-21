#lang racket

(require sicp)
(require rackunit)

(define modulus (make-parameter (expt 2 64)))
(define multiplier (make-parameter 6364136223846793005))
(define increment (make-parameter 1442695040888963407))

(define (rand-update x)
  (modulo (+ (* (multiplier) x) (increment)) (modulus)))

(define (square x) (* x x))
(define (cube x) (* x x x))
(define (average x y) (/ (+ x y) 2))
(define (divisible? a b) (= (remainder a b) 0))
(define (>= v1 v2) (or (> v1 v2) (= v1 v2)))

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
        [else (stream-filter predicate (stream-cdr s))]))

(define (scale-stream s factor)
  (stream-map (lambda (x) (* factor x)) s))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

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

(define (generate-random-numbers init)
  (cons-stream
   init
   (stream-map rand-update random-numbers)))
(define random-numbers (generate-random-numbers (rand-update 1)))

(define (map-successive-pairs f s)
  (cons-stream
   (f (stream-car s) (stream-ref s 1))
   (map-successive-pairs f (stream-cdr (stream-cdr s)))))

(define cesaro-stream
  (map-successive-pairs
   (lambda (prev next) (= (gcd prev next) 1))
   random-numbers))
  
(define (monte-carlo experiment-stream passed failed)
  (define (iter passed failed)
    (cons-stream
     (/ passed (+ passed failed))
     (monte-carlo (stream-cdr experiment-stream)
                  passed
                  failed)))
  (if (stream-car experiment-stream)
      (iter (+ passed 1) failed)
      (iter passed (+ failed 1))))

(define pi
  (stream-map
   (lambda (p) (sqrt (/ 6 p)))
   (monte-carlo cesaro-stream 0 0)))

; SOLUTION
(define (generate-resetable-random input-stream init-value)
  (define random
    (cons-stream
     init-value
     (stream-map (lambda (action prev)
                   (cond [(eq? (car action) 'reset) (cadr action)]
                         [(eq? (car action) 'generate) (rand-update prev)]
                         [else
                          (error
                           "Unknown action -- GENERATE-RESETABLE-RANDOM"
                           action)]))
                 input-stream
                 random)))
  random)

(define message-stream
  (cons-stream
   (list 'generate)
   (cons-stream
    (list 'generate)
    (cons-stream
     (list 'reset 10)
     (cons-stream
      (list 'generate)
      (cons-stream (list 'generate)
                   message-stream))))))

(define test (generate-resetable-random message-stream 10))

(check-equal? (stream-car test) 10)
(check-equal? (stream-ref test 3) 10)
(check-equal? (stream-ref test 1) (stream-ref test 4))
(check-equal? (stream-ref test 2) (stream-ref test 5))














