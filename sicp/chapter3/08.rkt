#lang sicp

(#%require rackunit)

(define (once)
  (let ((called? #f))
    (lambda (arg)
      (if called?
          0
          (begin (set! called? #t)
                 arg)))))
  

(define f (once))
(check-equal? (+ (f 0) (f 1)) 0)

(define g (once))
(check-equal? (+ (g 1) (g 0)) 1)
