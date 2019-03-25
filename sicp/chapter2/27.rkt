#lang sicp

(#%require rackunit)

(define (deep-reverse items)
  (define (iter acc rest)
    (if (not (pair? rest))
        (append acc rest)
        (iter (cons (deep-reverse (car rest)) acc)
              (cdr rest))))
  (iter '() items))

(check-equal? (deep-reverse '(1 2 3)) '(3 2 1))
(check-equal? (deep-reverse '((1 2) (3 4))) '((4 3) (2 1)))
(check-equal? (deep-reverse '((6 5) (4 3) 2 1)) '(1 2 (3 4) (5 6)))
