#lang sicp

(#%require rackunit)

; Tortoise and Hare algorithm

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define (check-infinite? seq)
  (define (last-pair? L)
    (or (null? L) (null? (cdr L))))
  (define (chase turtle rabbit)
    (cond ((eq? turtle rabbit) #t)
          ((last-pair? rabbit) #f)
          (else (chase (cdr turtle) (cddr rabbit)))))
  (if (last-pair? seq)
      #f
      (chase seq (cdr seq))))

(define l1 '(1 2 3 4 5 6))
(define l2 '(1 2 3 4 5 6))
(set-cdr! (last-pair l2) l2)
(define l3 '(1 2 3 4 5 6))
(set-cdr! (cdddr l3) l3)

(check-equal? (check-infinite? l1) #f)
(check-equal? (check-infinite? l2) #t)
(check-equal? (check-infinite? l3) #t)

