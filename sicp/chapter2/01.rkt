#lang sicp

(#%require rackunit)

(define (make-rat n d)
  (define same-sign?
    (or (and (> n 0) (> d 0)) (and (< n 0) (< d 0))))
  (let ((g (gcd n d))
        (sign (if same-sign? + -)))
    (cons (/ (sign (abs n)) g) (/ (abs d) g))))

(define (numer x) (car x))
(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (car x))
  (display "/")
  (display (cdr x)))

(define rat1 (make-rat -1 5))
(check-equal? (numer rat1) -1)
(check-equal? (denom rat1) 5)

(define rat2 (make-rat 1 -5))
(check-equal? (numer rat2) -1)
(check-equal? (denom rat2) 5)

(define rat3 (make-rat -1 -5))
(check-equal? (numer rat3) 1)
(check-equal? (denom rat3) 5)

(define rat4 (make-rat 1 5))
(check-equal? (numer rat4) 1)
(check-equal? (denom rat4) 5)


(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define rat5 (mul-rat (make-rat -3 5) (make-rat 2 -3)))
(check-equal? (numer rat5) 2)
(check-equal? (denom rat5) 5)

(define rat6 (mul-rat (make-rat -3 5) (make-rat 2 3)))
(check-equal? (numer rat6) -2)
(check-equal? (denom rat6) 5)

