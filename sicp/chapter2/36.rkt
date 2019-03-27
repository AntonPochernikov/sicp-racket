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

(define s '((1 2 3) (4 5 6) (7 8 9) (10 11 12)))
(check-equal? (accumulate-n + 0 s) '(22 26 30))

(define (zip seqs)
  (accumulate-n cons '() seqs))
(check-equal? (zip s) '((1 4 7 10) (2 5 8 11) (3 6 9 12)))
