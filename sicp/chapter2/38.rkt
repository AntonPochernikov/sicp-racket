#lang sicp

(#%require rackunit)

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define (fold-right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (fold-right op initial (cdr sequence)))))

(fold-right / 1 (list 1 2 3))
(fold-left / 1 (list 1 2 3))

(display (fold-right list nil (list 1 2 3)))
(newline)
(display (fold-left list nil (list 1 2 3)))
(newline)

(display "If result of the operation doesn't depend on arguments position, then both fold procedures will give us same result.")
(check-equal? (fold-right * 1 '(1 2 3)) (fold-left * 1 '(1 2 3)))