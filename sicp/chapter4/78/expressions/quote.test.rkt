#lang racket

(require rackunit)
(require "quote.rkt")

(check-true (quoted? '(quote ok)))
(check-equal? (text-of-quotation '(quote ok)) 'ok)
