#lang racket

(require sicp)
(require rackunit)
(require "../utils/tagged-list.rkt")

(provide assignment?
         assignment-variable
         assignment-value)

(define (assignment? exp) (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))
