#lang racket

(require sicp)
(require "../utils/tagged-list.rkt")

(provide make-if-fail
         if-fail?
         if-fail-consequent
         if-fail-alternative)

(define (make-if-fail consequent alternative)
  (list 'if-fail consequent alternative))

(define (if-fail? exp) (tagged-list? exp 'if-fail))
(define (if-fail-consequent exp)
  (cadr exp))
(define (if-fail-alternative exp)
  (caddr exp))






