#lang sicp

(#%require rackunit)

(define (pascal-triangle y x)
  (cond ((< y 3) 1)
        ((= x y) 1)
        ((= x 1) 1)
        (else (+ (pascal-triangle (- y 1) (- x 1))
                 (pascal-triangle (- y 1) x)))))

(check-equal? (pascal-triangle 1 1) 1)
(check-equal? (pascal-triangle 2 2) 1)
(check-equal? (pascal-triangle 3 2) 2)
(check-equal? (pascal-triangle 4 2) 3)
(check-equal? (pascal-triangle 5 2) 4)
(check-equal? (pascal-triangle 5 3) 6)

(display "all suits passed!")