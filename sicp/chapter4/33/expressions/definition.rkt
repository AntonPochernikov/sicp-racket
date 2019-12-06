#lang racket

(require sicp)
(require "../utils/tagged-list.rkt")
(require (only-in "lambda.rkt"
                  make-lambda))

(provide make-definition
         definition?
         definition-variable
         definition-value)

(define (make-definition var val)
  (list 'define var val))

(define (definition? exp)
  (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp) ; formal parameters
                   (cddr exp)))) ; body





