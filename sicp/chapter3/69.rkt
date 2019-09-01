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
    (pairs (stream-cdr s) t))))

; SOLUTION
(define (triples s t u)
  (let ([pairs-stream (pairs t u)])
    (cons-stream
     (list (stream-car s)
           (car (stream-car pairs-stream))
           (cadr (stream-car pairs-stream)))
     (interleave
      (stream-map
       (lambda (pair)
         (list (stream-car s) (car pair) (cadr pair)))
       (stream-cdr pairs-stream))
      (triples (stream-cdr s)
               (stream-cdr t)
               (stream-cdr u))))))

(display-stream (triples integers integers integers) 10)

(define pifagorus
  (stream-filter (lambda (triple) (= (square (caddr triple))
                                     (+ (square (car triple))
                                        (square (cadr triple)))))
                 (triples integers integers integers)))

(display-stream pifagorus 6)





























