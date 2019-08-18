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

(define (scale-stream s factor)
  (stream-map (lambda (x) (* factor x)) s))

(define ones (cons-stream 1 ones))
(define integers
  (cons-stream 1
               (add-streams integers ones)))

(define (merge s1 s2)
  (cond [(stream-null? s1) s2]
        [(stream-null? s2) s1]
        [else
         (let ([scar1 (stream-car s1)]
               [scar2 (stream-car s2)])
           (cond [(> scar1 scar2)
                  (cons-stream scar2
                               (merge s1 (stream-cdr s2)))]
                 [(< scar1 scar2)
                  (cons-stream scar1
                               (merge (stream-cdr s1) s2))]
                 [else
                  (cons-stream scar1
                               (merge (stream-cdr s1)
                                      (stream-cdr s2)))]))]))
; SOLUTION
(define hemmings
  (cons-stream 1
               (merge (scale-stream hemmings 2)
                      (merge (scale-stream hemmings 3)
                             (scale-stream hemmings 5)))))

(check-equal? (stream-ref hemmings 0) 1)
(check-equal? (stream-ref hemmings 1) 2)
(check-equal? (stream-ref hemmings 2) 3)
(check-equal? (stream-ref hemmings 3) 4)
(check-equal? (stream-ref hemmings 4) 5)
(check-equal? (stream-ref hemmings 5) 6)
(check-equal? (stream-ref hemmings 6) 8)
(check-equal? (stream-ref hemmings 7) 9)
(check-equal? (stream-ref hemmings 8) 10)
(check-equal? (stream-ref hemmings 9) 12)
(check-equal? (stream-ref hemmings 10) 15)






