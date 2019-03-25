#lang sicp

(define (for-each iteratee items)
  (cond ((null? items) (newline) #t)
        (else (iteratee (car items))
              (for-each iteratee (cdr items)))))

(for-each (lambda (x) (newline) (display x))
          `(57 321 88))
