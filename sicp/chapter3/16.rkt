#lang sicp

(#%require rackunit)

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

(define a (cons 4 5))
(define x (cons 2 3))
(define y (cons 1 x))
(define k (cons x x))

(define b (cons a x))
(define z (cons y x))
(define c (cons k k))

(check-equal? (count-pairs b) 3)
(check-equal? (count-pairs b) 4)
(check-equal? (count-pairs b) 7)

(define l (list 1 2 3))
(set-cdr! (last-pair l) l)
; example above will cause infinite recursion when counting pairs
