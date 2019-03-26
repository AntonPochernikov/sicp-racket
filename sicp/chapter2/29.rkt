#lang sicp

(#%require rackunit)

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch m)
  (car m))

(define (right-branch m)
  (cadr m))

(define (branch-length b)
  (car b))

(define (branch-structure b)
  (cadr b))

(define (total-weight m)
  (if (not (pair? m))
      m
      (let ((left (left-branch m))
            (right (right-branch m)))
        (+ (total-weight (branch-structure left))
           (total-weight (branch-structure right))))))

(define x (make-mobile (make-branch 2 1) (make-branch 10 (make-mobile (make-branch 4 2) (make-branch 6 3)))))
(check-equal? (total-weight x) 6)

(define (balanced? m)
  (if (not (pair? m))
      #t
      (let ((left (left-branch m))
            (right (right-branch m)))
        (and (= (* (branch-length left) (total-weight (branch-structure left)))
                (* (branch-length right) (total-weight (branch-structure right))))
             (balanced? (branch-structure left))
             (balanced? (branch-structure right))))))

(define a (make-mobile (make-branch 2 3) (make-branch 2 3)))
(define b (make-mobile (make-branch 2 3) (make-branch 4 5)))
(define c (make-mobile (make-branch 5 a) (make-branch 3 b)))
(define d (make-mobile (make-branch 10 a) (make-branch 12 5)))

(check-equal? (balanced? a) #t)
(check-equal? (balanced? b) #f)
(check-equal? (balanced? c) #f)
(check-equal? (balanced? d) #t)

(display "since only constructors and selectors know about how we put mobile together, only this procedures need to be changed")
(define (make-mobile-cons left right)
  (list left right))

(define (make-branch-cons length structure)
  (list length structure))

(define (left-branch-cons m)
  (car m))

(define (right-branch-cons m)
  (cdr m))

(define (branch-length-cons b)
  (car b))

(define (branch-structure-cons b)
  (cdr b))
