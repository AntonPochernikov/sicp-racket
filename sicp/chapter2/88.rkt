#lang sicp

(define (install-polynomial-package)
  ; rest operations

  (define (negate-terms terms)
    (if (empty-termlist? terms)
        the-empty-termlist
        (let ((first (first-term terms))
              (rest (rest-terms terms)))
          (adjoint-term (make-term (order first)
                                   (negate (coeff first)))
                        (negate-terms rest)))))
  (define (negate-poly poly)
    (define (negate-terms terms)
    (if (empty-termlist? terms)
        the-empty-termlist
        (let ((first (first-term terms))
              (rest (rest-terms terms)))
          (adjoint-term (make-term (order first)
                                   (negate (coeff first)))
                        (negate-terms rest)))))
    (tag (make-polynomial (variable poly)
                          (negate-terms (term-list poly)))))

  (put 'negate 'polynomial negate-poly)
  
  (define (sub-poly p1 p2)
    (tag (add-poly p1 (negate-poly (contents p2)))))

  (put 'sub ('polynomial 'polynomial) sub-poly)

  'done)

; we will also have add negate proc for every other type
(define (install-integer-package)
  ; rest operations
  (put 'negate
       'scheme-number
       (lambda (n) (- n)))
  'done)
(define (install-rational-package)
  ; rest operations
  (put 'negate
       'rational
       (lambda (rat)
         (make-rational (negate (numer rat))
                        (denom rat))))
  'done)
(define (install-complex-package)
  ; rest operations
  (put 'negate
       'complex
       (lambda (complex)
         (make-from-real-imag (negate (real-part complex))
                              (negate (imag-part complex)))))
  'done)
