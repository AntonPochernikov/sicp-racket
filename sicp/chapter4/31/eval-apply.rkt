#lang racket

(require sicp)

(provide apply-in-underlying-scheme
         eval-in-underlying-scheme)

(define (apply-in-underlying-scheme proc args) (apply proc args))
(define (eval-in-underlying-scheme exp env) (eval exp env))
