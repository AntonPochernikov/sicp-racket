#lang sicp

(#%require rackunit)

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define (contains? pair L)
  (cond ((null? L) #f)
        ((eq? (car L) pair) #t)
        (else (contains? pair (cdr L)))))

(define (count-pairs x)
  (define counted '())
  (define (been-before? p) (contains? p counted))
  (define (inner rest)
    (cond ((not (pair? rest)) 0)
          ((been-before? rest) 0)
          (else
           (begin
             (set! counted (append counted (list rest)))
             (+ (inner (car rest))
                (inner (cdr rest))
                1)))))
  (inner x))


(define a (cons 4 5))
(define x (cons 2 3))
(define y (cons 1 x))
(define k (cons x x))

(define b (cons a x))
(define z (cons y x))
(define c (cons k k))

(check-equal? (count-pairs b) 3)
(check-equal? (count-pairs b) 3)
(check-equal? (count-pairs b) 3)

(define l (list 1 2 3))
(set-cdr! (last-pair l) l)
(check-equal? (count-pairs l) 3)