#lang racket

(require sicp)

(define (analyze exp)
  (cond [(self-evaluating? exp) (analyze-self-evaluating exp)]
        [(quoted? exp) (analyze-quoted exp)]
        [(variable? exp) (analyze-variable exp)]
        [(assignment? exp) (analyze-assignment exp)]
        [(definition? exp) (analyze-definition exp)]
        [(if? exp) (analyze-if exp)]
        [(lambda? exp) (analyze-lambda exp)]
        [(begin? exp) (analyze-sequence (begin-actions exp))]
        [(cond? exp) (analyze (cond->if exp))]
        [(let? exp) (analyze (let->combination exp))]
        [(unless? exp) (analyze (unless->if exp))]
        [(application? exp) (analyze-application exp)]
        [else (error "Unknown expression type: ANALYZE" exp)]))

(define (unless? exp) (tagged-list? exp 'unless))
(define (unless-predicate exp) (cadr exp))
(define (unless-consequent exp) (cadddr exp))
(define (unless-alternative exp) (caddr exp))

(define (make-list predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (unless->if exp)
  (make-list (unless-predicate exp)
             (unless-consequent exp)
             (unless-alternative exp)))


; we won't be able to use unless in high order procedure as argument
; that way it won't be analyzed as special form but as a procedure
; this example will throw exeption of undefined variable "unless"
(define predicates '())
(define consequents '())
(define alternatives '())
(map unless predicates consequents alternatives)









