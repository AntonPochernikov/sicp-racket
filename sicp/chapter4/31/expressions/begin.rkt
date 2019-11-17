#lang racket

(require sicp)
(require "../utils/tagged-list.rkt")

(provide make-begin
         begin?
         begin-actions
         sequence->exp
         last-exp?
         first-exp
         rest-exps)

(define (make-begin seq) (cons 'begin seq))

(define (begin? exp)
  (tagged-list? exp 'begin))

(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond [(null? seq) seq]
        [(last-exp? seq) (first-exp seq)]
        [else (make-begin seq)]))
