#lang sicp

(#%require rackunit)

(define (filtered-accumulate predicate? combiner null-value term a next b)
  (define (iter n result)
    (if (> n b)
        result
        (iter (next n) (combiner
                        (if (predicate? n) (term n) null-value)
                        result))))
  (iter a null-value))

(define (square x) (* x x))

(define (smallest-divisor n)
  (define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
  (find-divisor n 2))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (sum-of-primes a b)
  (filtered-accumulate prime? + 0 square a inc b))

(check-equal? (sum-of-primes 1 5) 39)
(check-equal? (sum-of-primes 4 10) 74)

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (product-of-prime-to n)
  (define (prime-to? a)
    (= (gcd a n) 1))
  (filtered-accumulate prime-to? * 1 identity 1 inc n))

(check-equal? (product-of-prime-to 5) 24)
(check-equal? (product-of-prime-to 10) 189)
