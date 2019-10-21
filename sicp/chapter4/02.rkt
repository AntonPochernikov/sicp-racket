#lang racket

(require sicp)

; a)
; Our application? predicate checks if expression is pair.
; In our case every list is a pair.
; (define x 3) will be treated as application
; and therefore will be evaluated as follows:
; (apply (eval 'define env) (list-of-values '(3) env))
; (apply 'define '(x 3))
; that will give us an error cause we are applying symbol to arguments
(apply 'define '(x 3))


; b)
(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))
(define (application? exp) (tagged-list? exp 'call))
(define (operator exp) (cadr exp))
(define (operands exp) (cddr exp))








