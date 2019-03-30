#lang sicp

(#%require rackunit)

(define (equal? l1 l2)
  (cond ((and (null? l1) (null? l2)) #t)
        ((or (null? l1) (null? l2)) #f)
        ((and (pair? l1) (pair? l2))
         (and (equal? (car l1) (car l2))
              (equal? (cdr l1) (cdr l2))))
        (else (eq? l1 l2))))

(check-equal? (equal? '() '()) #t)
(check-equal? (equal? '() 'a) #f)
(check-equal? (equal? '((x1 x2) (y1 y2)) '((x1 x2) (y1 y2))) #t)
(check-equal? (equal? '((x1 x2) (y1 y2)) '((x1 x2 x3) (y1 y2))) #f)
(check-equal? (equal? '(x1 x2) 'y1) #f)
(check-equal? (equal? 'abc 'abc) #t)
(check-equal? (equal? 'abc 'abd) #f)
(check-equal? (equal? 123 123) #t)
(check-equal? (equal? 123 125) #f)
