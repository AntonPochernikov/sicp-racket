#lang racket

(require sicp)
(require rackunit)

(define (square x) (* x x))
(define (cube x) (* x x x))
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

(define (merge-weighted s1 s2 weight)
  (cond [(stream-null? s1) s2]
        [(stream-null? s2) s1]
        [else
         (let ([car1 (stream-car s1)]
               [car2 (stream-car s2)])
           (cond [(> (weight car1) (weight car2))
                  (cons-stream car2
                               (merge-weighted s1
                                               (stream-cdr s2)
                                               weight))]
                 [(< (weight car1) (weight car2))
                  (cons-stream car1
                               (merge-weighted (stream-cdr s1)
                                               s2
                                               weight))]
                 [else
                  (cons-stream car1
                               (merge-weighted (stream-cdr s1)
                                              s2
                                              weight))]))]))

(define (weighted-pairs s1 s2 weighting-proc)
  (cons-stream
   (list (stream-car s1) (stream-car s2))
   (merge-weighted
    (stream-map (lambda (x) (list (stream-car s1) x))
                (stream-cdr s2))
    (weighted-pairs (stream-cdr s1)
                    (stream-cdr s2)
                    weighting-proc)
    weighting-proc)))

; SOLUTION
(define (generate-sum-of-two-squares-3-ways)
  (let ([weight (lambda (pair)
                  (+ (square (car pair))
                     (square (cadr pair))))])
    (define pairs
      (weighted-pairs integers
                      integers
                      weight))
    (define (iter first second third S)
      (if (= (weight first) (weight second) (weight third))
          (cons-stream (list first second third (weight third))
                       (iter (stream-ref S 3)
                             (stream-ref S 4)
                             (stream-ref S 5)
                             (stream-cdr (stream-cdr (stream-cdr S)))))
          (iter (stream-ref S 1)
                (stream-ref S 2)
                (stream-ref S 3)
                (stream-cdr S))))
    (iter (stream-car pairs)
          (stream-ref pairs 1)
          (stream-ref pairs 2)
          pairs)))

(define sum-of-two-squares-3-ways (generate-sum-of-two-squares-3-ways))

(display-stream sum-of-two-squares-3-ways 6)














