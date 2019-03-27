#lang sicp

(#%require rackunit)
(define (enumerate-interval from to)
  (if (> from to)
      nil
      (cons from (enumerate-interval (inc from) to))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (flatmap op seq)
  (accumulate append nil (map op seq)))

(define (filter predicate? seq)
  (cond ((null? seq) nil)
        ((predicate? (car seq)) (cons (car seq) (filter predicate? (cdr seq))))
        (else (filter predicate? (cdr seq)))))

(define (unique-triples n)
  (flatmap (lambda (i)
                     (flatmap (lambda (j)
                                (map (lambda (k) (list i j k))
                                     (enumerate-interval 1 (- j 1))))
                              (enumerate-interval 1 (- i 1))))
                   (enumerate-interval 1 n)))

(define (triple-by-sum n s)
  (filter (lambda (triple)
            (= s (accumulate + 0 triple)))
          (unique-triples n)))

(display (triple-by-sum 5 10))
