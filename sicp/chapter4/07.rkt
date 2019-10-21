#lang racket

(require sicp)

; LAMBDA
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))
(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

; LET
(define (let-clauses exp) (cadr exp))
(define (let-body exp) (caddr exp))

(define (first-clause clauses) (car clauses))
(define (rest-clauses clauses) (cdr clauses))
(define (empty-clauses? clauses) (null? clauses))
(define (last-clause? clauses)
  (empty-clauses? (rest-clauses clauses)))

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
  (list (make-lambda (let-params exp)
                     (let-body exp))
        (let-args exp)))

(define (make-let clauses body)
  (list 'let clauses body))

(define (eval-let exp env) (eval (let->combination exp) env))

; SOLUTION
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

(put 'eval 'let* eval-let*)
; we can impement let* as derived expression of let








