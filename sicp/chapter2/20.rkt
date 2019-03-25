#lang sicp

(#%require rackunit)

(define (same-parity . l)
  (define (filter l predicate?)
      (cond ((null? l) '())
            ((predicate? (car l)) (cons (car l) (filter (cdr l) predicate?)))
            (else (filter (cdr l) predicate?))))
  (if (null? l)
      '()
      (if (odd? (car l))
          (filter l odd?)
          (filter l even?))))

(check-equal? (same-parity 1 2 3 4 5 6 7) (list 1 3 5 7))

(check-equal? (same-parity 4 8 15 16 23 42) (list 4 8 16 42))

(check-equal? (same-parity) '())
