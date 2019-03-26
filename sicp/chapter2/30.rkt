#lang sicp

(#%require rackunit)

(define (square x) (* x x))

(define (square-tree tree)
  (map (lambda (sub-tree)
         (if (not (pair? sub-tree))
                  (square sub-tree)
                  (square-tree sub-tree))) tree))

(define x '(1 (2 (3 4) 5) (6 7)))
(define y '(1 (2 (3 (4 (5 (6 7)))))))
(define z '(((((((1) 2) 3) 4) 5) 6) 7))

(check-equal? (square-tree x) '(1 (4 (9 16) 25) (36 49)))
(check-equal? (square-tree y) '(1 (4 (9 (16 (25 (36 49)))))))
(check-equal? (square-tree z) '(((((((1) 4) 9) 16) 25) 36) 49))

(define (square-tree-2 tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (square tree))
        (else (cons (square-tree-2 (car tree))
                    (square-tree-2 (cdr tree))))))

(check-equal? (square-tree-2 x) '(1 (4 (9 16) 25) (36 49)))
(check-equal? (square-tree-2 y) '(1 (4 (9 (16 (25 (36 49)))))))
(check-equal? (square-tree-2 z) '(((((((1) 4) 9) 16) 25) 36) 49))
