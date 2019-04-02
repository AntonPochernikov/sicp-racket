#lang sicp

(#%require rackunit)

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((< x (car set)) false)
        ((= x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((= (car set) x) set)
        ((> (car set) x) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(check-equal? (adjoin-set 1 '()) '(1))
(check-equal? (adjoin-set 1 '(2 3 4 5)) '(1 2 3 4 5))
(check-equal? (adjoin-set 3 '(2 3 4 5)) '(2 3 4 5))
(check-equal? (adjoin-set 7 '(2 3 4 5)) '(2 3 4 5 7))
(check-equal? (adjoin-set 3 '(1 2 4 5)) '(1 2 3 4 5))
