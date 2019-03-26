#lang sicp

(#%require rackunit)

(define (tree-map iteratee tree)
  (map (lambda (node)
         (if (pair? node)
             (tree-map iteratee node)
             (iteratee node))) tree))

(define (square x) (* x x))
(define (square-tree tree) (tree-map square tree))

(define x '(1 (2 (3 4) 5) (6 7)))
(define y '(1 (2 (3 (4 (5 (6 7)))))))
(define z '(((((((1) 2) 3) 4) 5) 6) 7))

(check-equal? (square-tree x) '(1 (4 (9 16) 25) (36 49)))
(check-equal? (square-tree y) '(1 (4 (9 (16 (25 (36 49)))))))
(check-equal? (square-tree z) '(((((((1) 4) 9) 16) 25) 36) 49))
