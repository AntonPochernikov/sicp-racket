#lang racket

(require rackunit)

(define (f n)
  (cond ((< n 3) n)
        (else (+ (f (- n 1))
                 (f (- n 2))
                 (f (- n 3))))))
(check-equal? (f 1) 1)
(check-equal? (f 2) 2)
(check-equal? (f 3) 3)
(check-equal? (f 5) 11)
(check-equal? (f 10) 230)

(define (g n)
  (define (iter a b c count)
    (if (= count 0) c
        (iter (+ a b c) a b (- count 1))))
  (iter 2 1 0 n))

(check-equal? (g 1) 1)
(check-equal? (g 2) 2)
(check-equal? (g 3) 3)
(check-equal? (g 5) 11)
(check-equal? (g 10) 230)