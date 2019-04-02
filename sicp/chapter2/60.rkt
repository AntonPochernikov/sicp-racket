#lang sicp

(#%require rackunit)

(define x '(2 3 2 1 3 2 2))

; element-of-set? remains unchanged
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

; now we don't have to perform element-of-set? check for new element in adjoin procedure
(define (adjoin-set x set)
  (cons x set))
(check-equal? (adjoin-set 1 x) '(1 2 3 2 1 3 2 2))

; intersection-set remains unchanged
(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

; now we don't have to perform element-of-set? check for new element in union procedure
(define (union-set set1 set2)
  (append set1 set2))
(check-equal? (union-set '(1 2 3 2 1 3 2 2) '(1 2 3)) '(1 2 3 2 1 3 2 2 1 2 3))

; adjoin and union procuders are faster now since we removed element-of-set? check in their implementation
; however the amount of memory used by every set is growing every time we are using union and adjoin