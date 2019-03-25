#lang sicp

(#%require rackunit)

(define (last-pair l)
  (if (null? l)
      nil
      (list-ref l (- (length l) 1))))

(check-equal? (last-pair (list 23 72 149 34)) 34)
(check-equal? (last-pair (list)) nil)
