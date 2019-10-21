#lang racket

(require sicp)

; DEFINITION
(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))
(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)   ; formal parameters
                   (cddr exp)))) ; body
(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (eval (definition-value exp) env)
                    env)
  'ok)
(define (make-definition variable value)
  (list 'define variable value))

; SOLUTION
(define (make-let clauses body)
  (cons 'let (cons clauses body)))
(define (let? exp) (tagged-list? exp 'let))

(define (let-named? exp)
  (symbol? (cadr exp)))
(define (let-clauses exp)
  (if (let-named? exp)
      (caddr exp)
      (cadr exp)))
(define (let-body exp)
  (if (let-named? exp)
      (cdddr exp)
      (cddr exp)))
(define (let-name exp)
  (if (let-named? exp)
      (cadr exp)
      (error "Not named let expression: LET" exp)))

(define (first-clause clauses) (car clauses))
(define (rest-clauses clauses) (cdr clauses))
(define (empty-clauses? clauses) (null? clauses))
(define (last-clause? clauses) (null? (cdr clauses)))

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
  (if (let-named? exp)
      (list (make-lambda '()
                         (list
                          ; definition
                          (make-definition (let-name exp)
                                           (make-lambda (let-params exp)
                                                        (let-body exp)))
                          ; application
                          (cons (let-name exp) (let-args exp)))))
      (cons (make-lambda (let-params exp)
                         (let-body exp))
            (let-args exp))))

(define (eval-let exp env)
  (eval (let->combination exp) env))















