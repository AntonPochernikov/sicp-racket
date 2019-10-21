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
  
(define (monte-carlo experiment-stream passed failed)
  (define (iter passed failed)
    (cons-stream
     (/ passed (+ passed failed))
     (monte-carlo (stream-cdr experiment-stream)
                  passed
                  failed)))
  (if (stream-car experiment-stream)
      (iter (+ passed 1.0) failed)
      (iter passed (+ failed 1.0))))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

; SOLUTION
(define (estimate-integral predicate x1 x2 y1 y2)
  (define area
    (* (abs (- x2 x1))
       (abs (- y2 y1))))
  (define (generate-experiment-stream)
    (cons-stream
     (predicate (random-in-range x1 x2)
                (random-in-range y1 y2))
     (generate-experiment-stream)))

  (scale-stream (monte-carlo (generate-experiment-stream) 0 0) area))

(define (in-circle r cx cy)
  (lambda (x y)
    (let ((left (+ (square (- x cx))
                   (square (- y cy))))
          (right (square r)))
      (or (< left right)
          (= left right)))))

(define P (in-circle 1 1 1))


(display-stream (estimate-integral P 0 2 0 2) 100)












