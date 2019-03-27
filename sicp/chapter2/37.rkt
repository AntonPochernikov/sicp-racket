#lang sicp

(#%require rackunit)

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
             nil
             (cons (accumulate op init (map (lambda (seq) (car seq)) seqs))
                   (accumulate-n op init (map (lambda (seq) (cdr seq)) seqs)))))

(define (dot-product v w)
  (accumulate + 0 (accumulate-n * 1 (list v w))))

(define (matrix-*-vector m v)
  (map (lambda (row) (dot-product row v)) m))

(define a '((2 4 0) (-2 1 3) (-1 0 1)))
(define b '(1 2 -1))

(check-equal? (matrix-*-vector a b) '(10 -3 -2))

(define (transpose m)
  (accumulate-n cons nil m))

(check-equal? (transpose a) '((2 -2 -1) (4 1 0) (0 3 1)))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row)
           (matrix-*-vector cols row)) m)))

(check-equal? (matrix-*-matrix a a) '((-4 12 12) (-9 -7 6) (-3 -4 1)))
