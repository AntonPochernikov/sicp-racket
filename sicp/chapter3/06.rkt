#lang sicp

(#%require rackunit)

(define random-init 0)
(define (rand-update x) (inc x))

(define rand
  (let ((x random-init))
    (define (dispatch m)
      (cond ((eq? m 'generate)
             (begin (set! x (rand-update x))
                    x))
            ((eq? m 'reset)
             (lambda (new)
               (begin (set! x new)
                      x)))
            (else
             (error "Unknown command -- RAND" m))))
    dispatch))

(check-equal? (rand 'generate) 1)
(check-equal? (rand 'generate) 2)
(check-equal? (rand 'generate) 3)
(check-equal? ((rand 'reset) 0) 0)
(check-equal? (rand 'generate) 1)
