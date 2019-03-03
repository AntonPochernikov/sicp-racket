#lang sicp

(#%require rackunit)
(define (square x) (* x x))
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))
  
(define (fermat-test n a)
  (= (expmod a n n) a))

(define (test-karmichael n)
  (define (iter counter)
    (cond ((not (fermat-test n counter)) false)
          ((<= counter 1) true)
          (else (iter (- counter 1)))))
  (iter (- n 1)))

(check-equal? (test-karmichael 561) #t)
(check-equal? (test-karmichael 1105) #t)
(check-equal? (test-karmichael 1729) #t)
(check-equal? (test-karmichael 2465) #t)
(check-equal? (test-karmichael 2821) #t)
(check-equal? (test-karmichael 6601) #t)
(display "all tests are done!")
