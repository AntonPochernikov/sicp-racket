#lang racket

(require sicp)
(require rackunit)
(require "../utils/tagged-list.rkt")
(require "../utils/filter.rkt")
(require (only-in "definition.rkt"
                  make-definition
                  definition?
                  definition-variable
                  definition-value))
(require (only-in "let.rkt" make-let))
(require (only-in "../environment.rkt"
                  the-empty-environment
                  extend-environment))

(define (make-procedure parameters body env)
  (list 'procedure
        parameters
        (scan-out-defines body)
        env))

(define (compound-procedure? p) (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))
(define (primitive-implementation proc) (cadr proc))

(define (scan-out-defines body)
  (let ([definitions (filter definition? body)]
        [rest-body (filter (lambda (exp)
                        (not (definition? exp)))
                      body)])
    (if (null? definitions)
        body
        (let ([vars (map definition-variable definitions)]
              [vals (map definition-value definitions)])
          (make-let (map (lambda (var)
                           (list var '*unassigned*))
                         vars)
                    (append
                     (map (lambda (var val)
                            (list 'set! var val))
                          vars
                          vals)
                     rest-body))))))

(define test-env (extend-environment '()
                                     '()
                                     the-empty-environment))

(define test-proc
  (make-procedure '(x y z)
                  '((define sum +) (+ x y z))
                  test-env))

(check-true (compound-procedure? test-proc))
(check-equal? (procedure-parameters test-proc) '(x y z))
(check-equal? (procedure-body test-proc)
              '(let ((sum *unassigned*))
                 (set! sum +)
                 (+ x y z)))


