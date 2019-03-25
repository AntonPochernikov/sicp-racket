#lang sicp

(#%require rackunit)

(define x (list (list 1 2) (list 3 4)))
(define y '(1 (2 3) ((4) 5 (6 7))))

(define (fringe items)
  (cond ((null? items) nil)
        ((not (pair? items)) (list items))
        (else (append (fringe (car items))
                      (fringe (cdr items))))))

(check-equal? (fringe (list x x)) `(1 2 3 4 1 2 3 4))
(check-equal? (fringe y) '(1 2 3 4 5 6 7))
