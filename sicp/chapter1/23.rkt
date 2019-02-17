#lang sicp

(#%require rackunit)

(define (square x) (* x x))
(define (divides? a b) (= (remainder a b) 0))

(define (smallest-divisor n)
  (define (next prev) (if (= prev 2) 3 (+ prev 2)))
  (define (iter counter)
    (cond ((> (square counter) n) n)
          ((divides? n counter) counter)
          (else (iter (next counter)))))
  (iter 2))

(define (prime? n) (= n (smallest-divisor n)))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (search-for-primes start end)
  (if (even? start)
      (search-for-primes (+ start 1) end)
      (cond ((< start end) (timed-prime-test start)
                           (search-for-primes (+ start 2) end)))))

(search-for-primes 10000000000 10000000100)
