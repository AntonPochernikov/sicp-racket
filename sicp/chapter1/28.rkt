#lang sicp

(#%require rackunit)
(define (square x) (* x x))
(define (square-check x m)
  (if (and (not (or (= x 1) (= x (- m 1))))
           (= (remainder (* x x) m) 1))
      0
      (remainder (* x x) m)))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp) (remainder (square-check (expmod base (/ exp 2) m) m)
                                m))
        (else (remainder (* base (expmod base (- exp 1) m))
                         m))))

(define (miller-rabin-test n)
  (define (try-it a)
    (= (expmod a (- n 1) n) 1))
  (try-it (+ 2 (random (- n 2)))))

(check-equal? (miller-rabin-test 199) #t)
(check-equal? (miller-rabin-test 561) #f)
(check-equal? (miller-rabin-test 1105) #f)
(check-equal? (miller-rabin-test 1729) #f)
(check-equal? (miller-rabin-test 2465) #f)
(check-equal? (miller-rabin-test 2821) #f)
(check-equal? (miller-rabin-test 6601) #f)
(display "all tests are done!")