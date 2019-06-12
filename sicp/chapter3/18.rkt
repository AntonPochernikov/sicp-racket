#lang sicp

(#%require rackunit)

(define (check-infinite? L)
  (define encountered  '())
  (define (contains? pair L)
  (cond ((null? L) #f)
        ((eq? (car L) pair) #t)
        (else (contains? pair (cdr L)))))
  (define (iter rest)
    (cond ((null? rest) #f)
          ((contains? rest encountered ) #t)
          (else
           (begin
             (set! encountered  (cons rest encountered))
             (iter (cdr rest))))))
  (iter L))


(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define l '(1 2 3))
(set-cdr! (last-pair l) l)

(check-equal? (check-infinite? l) #t)
(check-equal? (check-infinite? '(1 2 3)) #f)

(define x '(a b c))
(define y '(d e f))
(set-car! (cdr x) y)
(set-car! x (cdr x))
(set-cdr! (last-pair y) (cdr y))

(check-infinite? y)
