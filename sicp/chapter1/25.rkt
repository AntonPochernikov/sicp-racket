#lang sicp

(#%require rackunit)
(define (square x) (* x x))
(define (-- x) (- x 1))

(define (fast-expt x n)
  (define (iter acc rest prod)
    (cond ((= rest 0) acc)
          ((even? rest) (iter acc (/ rest 2) (square prod)))
          (else (iter (* acc prod) (-- rest) prod))))
  (if (= n 0) 1 (iter 1 n x)))

(define (expmod base exp m)
  (remainder (fast-expt base exp) m))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
(cond ((= times 0) true)
((fermat-test n) (fast-prime? n (- times 1)))
(else false)))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (start-prime-test n start-time)
  (if (fast-prime? n 1)
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

(search-for-primes 100000 100020)
(display "too long because of squaring too large numbers")