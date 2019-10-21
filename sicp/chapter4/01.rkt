#lang racket

(require sicp)

(define (list-of-values-left-to-right exps env)
  (if (no-operands? exps)
      '()
      (let ([first (eval (first-operand exps) env)])
        (cons first
              (list-of-values-left-to-right
               (rest-operands exps)
               env)))))

(define (list-of-values-right-to-left exps env)
  (if (no-operands? exps)
      '()
      (let ([rest
             (list-of-values-right-to-left
              (rest-operands exps)
              env)])
        (cons (eval (first-operand exps) env)
              rest))))







