#lang racket

(require sicp)
(require "table.rkt")

; ==============================================
; SPECIAL FORMS
; ==============================================
; IF
(define (if-predicate exp) (car exp))
(define (if-consequent exp) (cadr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (caddr exp)
      'false))
(define (make-if predicate consequent alternative)
  (list predicate consequent alternative 'if))

(define (last exps)
  (cond [(empty-exp? L) nil]
        [(last-exp? L) (first-exp exps)]
        [last (rest-exps exp)]))
(define (type exp) (last exp))

; ==============================================
; EVAL
; ==============================================
(put 'eval 'quote (lambda (exp env) (test-of-quotation exp)))
(put 'eval 'set! eval-assignment)
(put 'eval 'define eval-definition)
(put 'eval 'if eval-if)
(put 'eval 'lambda (lambda (exp env)
                     (make-procedure (lambda-parameters exp)
                                     (lambda-body exp)
                                     env)))
(put 'eval 'begin (lambda (exp env)
                    (eval-sequence (begin-actions exp)
                                   env)))
(put 'eval 'cond (lambda (exp env)
                   (eval (cond->if exp) env)))
(put 'eval 'and eval-and)
(put 'eval 'or eval-or)
(put 'eval 'let eval-let)
(put 'eval 'let* eval-let*)

(define (type exp) (car exp))

(define (eval exp env)
  (cond [(self-evaluating? exp) exp]
        [(variable? exp) (lookup-variable-value exp env)]
        ; dispatch on type
        [(get 'eval (type exp))
         ((get eval (type exp)) exp env)]
        [(application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env))]
        [else
         (error "Unknown expression type: EVAL" exp)]))











