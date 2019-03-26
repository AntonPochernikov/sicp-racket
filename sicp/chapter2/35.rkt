#lang sicp

(#%require rackunit)

(define (enumerate-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (count-leaves t)
  (accumulate (lambda (x y) (+ 1 y))
              0
              (enumerate-tree t)))

(define x '(1 (2 (3 4) 5 (6 7))))

(check-equal? (count-leaves x) 7)
  