#lang racket

(require sicp)
(require "../utils/tagged-list.rkt")

(provide make-if
         if?
         if-predicate
         if-consequent
         if-alternative)

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (if? exp)
  (tagged-list? exp 'if))

(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))
