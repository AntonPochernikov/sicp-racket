#lang sicp

(define (=zero? x)
  (apply-generic '=zero? x))

(define (install-scheme-number-package)
  ; rest package procedures
  (put '=zero? '(scheme-number) =zero?)
  'done)

(define (install-rational-package)
  ; rest package procedures
  (define (=zero-rat? x)
    (=zero? (numer x)))
  (put '=zero? '(rational) =zero-rat?)
  'done)

(define (install-complex-package)
  ; rest package procedures
  (define (=zero-complex? x)
    (and (=zero? (real-part rat))
         (=zero? (imag-part rat))))
  (put '=zero? '(complex) =zero-complex?)
  'done)
