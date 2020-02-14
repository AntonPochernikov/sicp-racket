#lang racket

(require sicp)
(require "../utils/tagged-list.rkt")
(provide quoted? text-of-quotation)

(define (quoted? exp) (tagged-list? exp 'quote))
(define (text-of-quotation exp) (cadr exp))
