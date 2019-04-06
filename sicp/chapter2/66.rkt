#lang sicp

(#%require rackunit)

(define (make-tree entry left right)
  (list entry left right))
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))

(define (lockup given-key set-of-records)
  (define key identity)
  (define record identity)
  (cond ((null? set-of-records) false)
        ((equal? given-key (key (entry set-of-records)))
         (record (entry set-of-records)))
        ((< given-key (key (entry set-of-records)))
         (lockup given-key(left-branch set-of-records)))
        ((> given-key (key (entry set-of-records)))
         (lockup given-key (right-branch set-of-records)))))

(define x (make-tree 7
                     (make-tree 3
                                (make-tree 1 '() '())
                                (make-tree 5 '() '()))
                     (make-tree 9
                                '()
                                (make-tree 11 '() '()))))

(check-equal? (lockup 5 x) 5)
(check-equal? (lockup 6 x) false)
