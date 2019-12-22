#lang racket

(require sicp)

; SOLUTION
(define (permanent-assignment? exp)
  (tagged-list? exp 'permanent-set!))

(define (analyze exp)
  (cond [(self-evaluating? exp) (analyze-self-evaluating exp)]
        [(quoted? exp) (analyze-quoted exp)]
        [(variable? exp) (analyze-variable exp)]
        [(assignment? exp) (analyze-assignment exp)]
        ; adding permanent assignment as a special form
        [(permanent-assignment? exp) (analyze-permanent-assignment exp)]
        [(definition? exp) (analyze-definition exp)]
        [(if? exp) (analyze-if exp)]
        [(lambda? exp) (analyze-lambda exp)]
        [(begin? exp) (analyze-sequence (begin-actions exp))]
        [(cond? exp) (analyze (cond->if exp))]
        [(let? exp) (analyze (let->combination exp))]
        [(amb? exp) (analyze-amb exp)]
        [(ramb? exp) (analyze-ramb exp)]
        [(application? exp) (analyze-application exp)]
        [else (error "Unknown expression type: ANALYZE" exp)]))

(define (analyze-permanent-assignment exp)
  (let ([variable (assignment-variable exp)]
        [vproc (analyze (assignment-value exp))])
    (lambda (env succeed fail)
      (vproc env
             (lambda (value fail2)
               (set-variable-value! variable
                                    value
                                    env)
               (succeed 'ok fail2))
             fail))))



; If we had used set! here rather than permanent-set!
; we would get count = 1 on every try.






