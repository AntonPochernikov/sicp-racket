#lang sicp

(#%require rackunit)

(check-equal? (car (cdaddr '(1 3 (5 7) 9))) 7)
(check-equal? (caar '((7))) 7)
(check-equal? (cadadr (cadadr (cadadr '(1 (2 (3 (4 (5 (6 7))))))))) 7)
