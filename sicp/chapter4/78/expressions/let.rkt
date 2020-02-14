#lang racket

(require sicp)
(require "../utils/tagged-list.rkt")
(require (only-in "lambda.rkt" make-lambda))
(require (only-in "definition.rkt" make-definition))

(provide make-let
         let?
         let->combination
         let*?
         let*->nested-lets
         letrec?
         letrec->let)

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


(define (let*? exp) (tagged-list? exp 'let*))

(define (let*->nested-lets exp)
  (let*-expand-clauses (let-clauses exp)
                       (let-body exp)))

(define (let*-expand-clauses clauses body)
  (cond [(empty-clauses? clauses) body]
        [(last-clause? clauses)
         (make-let (list (first-clause clauses)) body)]
        [else
         (make-let (list (first-clause clauses))
                   (list (let*-expand-clauses (rest-clauses clauses)
                                              body)))]))

(define (letrec? exp) (tagged-list? exp 'letrec))

(define (letrec->let exp)
  (let ([clauses (let-clauses exp)]
        [body (let-body exp)])
    (if (empty-clauses? clauses)
        body
        (let ([params (map clause-param clauses)]
              [args (map clause-arg clauses)])
          (make-let (map (lambda (p) (list p '*unassigned*))
                         params)
                    (append
                     (map (lambda (p a) (list 'set! p a))
                          params
                          args)
                     body))))))










