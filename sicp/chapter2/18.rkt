#lang sicp

(#%require rackunit)

(define (reverse l)
  (define (iter acc rest)
    (if (null? rest)
        acc
        (iter (cons (car rest) acc) (cdr rest))))
  (iter (list) l))

(check-equal? (reverse (list 1 2 3 4 5)) (list 5 4 3 2 1))
(check-equal? (reverse (list 23 72 149 34)) (list 34 149 72 23))
