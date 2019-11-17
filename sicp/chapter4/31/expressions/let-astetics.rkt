#lang racket

(require sicp)
(require rackunit)
(require "../utils/tagged-list.rkt")

(define (let*? exp) (tagged-list? exp 'let*))
(define (let*-clauses exp) (cadr exp))
(define (let*-body exp) (caddr exp))
(define (let*-param clause) (car clause))
(define (let*-arg clause) (cadr clause))
(define (let*->nested-lets exp)
  (let*-expand-clauses (let*-clauses exp)
                       (let*-body exp)))
(define (let*-expand-clauses clauses body)
  (cond [(empty-clauses? clauses) body]
        [(last-clause? clauses)
         (make-let (list (first-clause clauses)) body)]
        [else
         (make-let (list (first-clause clauses))
                   (let*-expand-clauses (rest-clauses clauses)
                                        body))]))

(define (eval-let* exp env)
  (eval (let*->nested-lets exp) env))