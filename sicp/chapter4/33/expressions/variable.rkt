#lang racket

(provide variable?)

(define (variable? exp) (symbol? exp))
