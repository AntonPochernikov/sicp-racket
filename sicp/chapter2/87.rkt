#lang sicp

(define (install-polynomial-package)
  ; rest operations

  (define (=zero-poly? poly)
    (define (zero-terms? terms)
      (or (empty-termlist? terms)
          (and (=zero? (coeff (first-term terms)))
               (zero-terms? (rest-terms terms)))))
    (check-terms (term-list poly)))

  (put '=zero? 'polynomial =zero-poly?)

  'done)
