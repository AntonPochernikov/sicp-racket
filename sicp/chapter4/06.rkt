#lang racket

(require sicp)

; LAMBDA
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))
(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

; SOLUTION
(define (let? exp) (tagged-list? exp 'let))
(define (let-clauses exp) (cadr exp))
(define (let-body exp) (cddr exp))

(define (first-clause clauses) (car clauses))
(define (rest-clauses clauses) (cdr clauses))
(define (empty-clauses? clauses) (null? clauses))

(define (clause-param clause) (car clause))
(define (clause-arg clause) (cadr clause))

(define (let-params exp)
  (define (iter clauses)
    (if (empty-clauses? clauses)
        '()
        (cons (clause-param (first-clause clauses))
              (iter (rest-clauses clauses)))))
  (iter (let-clauses exp)))

(define (let-args exp)
  (define (iter clauses)
    (if (empty-clauses? clauses)
        '()
        (cons (clause-arg (first-clause clauses))
              (iter (rest-clauses clauses)))))
  (iter (let-clauses exp)))

(define (let->combination exp)
  (cons (make-lambda (let-params exp)
                     (let-body exp))
        (let-args exp)))

(define (eval-let exp env) (eval (let->combination exp) env))











