#lang racket

(require sicp)

(define (analyze exp)
  (cond [(self-evaluating? exp) (analyze-self-evaluating exp)]
        [(quoted? exp) (analyze-quoted exp)]
        [(variable? exp) (analyze-variable exp)]
        [(assignment? exp) (analyze-assignment exp)]
        [(permanent-assignment? exp) (analyze-permanent-assignment exp)]
        [(definition? exp) (analyze-definition exp)]
        [(if? exp) (analyze-if exp)]
        [(lambda? exp) (analyze-lambda exp)]
        [(begin? exp) (analyze-sequence (begin-actions exp))]
        [(cond? exp) (analyze (cond->if exp))]
        [(let? exp) (analyze (let->combination exp))]
        [(amb? exp) (analyze-amb exp)]
        [(ramb? exp) (analyze-ramb exp)]
        ; add if-fail as special form
        [(if-fail? exp) (analyze-if-fail exp)]
        [(application? exp) (analyze-application exp)]
        [else (error "Unknown expression type: ANALYZE" exp)]))

(define (analyze-if-fail exp)
  (let ([consequent (analyze (if-fail-consequent exp))]
        [alternative (analyze (if-fail-alternative exp))])
    (lambda (env succeed fail)
      (consequent env
                  succeed
                  (lambda ()
                    (alternative env
                                 succeed
                                 fail))))))

(define (if-fail? exp) (tagged-list? exp 'if-fail))
(define (if-fail-consequent exp)
  (cadr exp))
(define (if-fail-alternative exp)
  (caddr exp))









