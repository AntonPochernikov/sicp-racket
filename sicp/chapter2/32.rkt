#lang sicp

(#%require rackunit)

(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s)))
            (head (car s)))
        (append rest (map (lambda (item) (cons head item)) rest)))))

(check-equal? (subsets '(1 2 3)) '(() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3)))
(check-equal? (subsets '(2 3)) '(() (3) (2) (2 3)))
