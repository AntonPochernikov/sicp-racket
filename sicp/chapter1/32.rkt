#lang sicp

(#%require rackunit)

(define (accumulate-recursive combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate-recursive combiner
                                      null-value
                                      term
                                      (next a)
                                      next
                                      b))))
(define (accumulate-iterative combiner null-value term a next b)
  (define (iter n result)
    (if (> n b)
        result
        (iter (next n) (combiner (term n) result))))
  (iter a null-value))

(define (sum-recursive term a next b)
  (accumulate-recursive + 0 term a next b))
(define (sum-iterative term a next b)
  (accumulate-iterative + 0 term a next b))

(define (square x) (* x x))

(check-equal? (sum-iterative identity 1 inc 5) 15)
(check-equal? (sum-iterative square 4 inc 5) 41)
(check-equal? (sum-iterative identity 1 inc 5) 15)
(check-equal? (sum-iterative square 4 inc 5) 41)

(define (product-recursive term a next b)
  (accumulate-recursive * 1 term a next b))
(define (product-iterative  term a next b)
  (accumulate-iterative * 1 term a next b))

(define (factorial-recursive n)
  (product-recursive identity 1 inc n))
(define (factorial-iterative n)
  (product-recursive identity 1 inc n))

(check-equal? (factorial-recursive 5) 120)
(check-equal? (factorial-iterative 5) 120)

