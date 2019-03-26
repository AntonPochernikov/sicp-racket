#lang sicp

(#%require rackunit)

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) nil sequence))

(define x '(1 2 3 4 5))
(define (square x) (* x x))
(check-equal? (map square x) '(1 4 9 16 25))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))
(define y '(6 7 8))
(define z (append x y))
(check-equal? z '(1 2 3 4 5 6 7 8))

(define (length sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

(check-equal? (length z) 8)
