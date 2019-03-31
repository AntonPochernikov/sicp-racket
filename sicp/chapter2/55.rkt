#lang sicp

(#%require rackunit)

(car ''abracadabra)

; this expression is a shorthand for (car (quote (quote abracadabra)))
; so the car result will be car for '(quote abracadabra) => 'quote
