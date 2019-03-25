#lang sicp

(#%require rackunit)

(define (cc amount coin-values)
  (define (no-more? l) (null? l))
  (define (first-denomination l) (car l))
  (define (except-first-denomination l) (cdr l))
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount (except-first-denomination coin-values))
            (cc (- amount (first-denomination coin-values)) coin-values)))))

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(check-equal? (cc 100 us-coins) 292)
(check-equal? (cc 20 uk-coins) 293)
