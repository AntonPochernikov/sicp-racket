#lang sicp

(define (equ? x y)
  (apply-generic 'equ? x y))

(define (install-scheme-number-package)
  ; rest package procedures
  (put 'equ? '(scheme-number scheme-number) =)
  'done)

(define (install-rational-package)
  ; rest package procedures
  (define (equal-rat x y)
    (= (* (numer x) (denom y))
       (* (numer y) (denom x))))
  (put 'equ? '(rational rational) equal-rat)
  'done)

(define (install-complex-package)
  ; rest package procedures
  (define (equal-complex x y)
    (and (= (real-part x) (real-part y)) (= (imag-part x) (imag-part y))))
  (put 'equ? '(complex complex) equal-complex)
  'done)
