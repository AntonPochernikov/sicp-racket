#lang racket

(require rackunit)
(require "index.rkt")

(check-equal? (eval '(square 5)
                    the-global-environment) 25)

















