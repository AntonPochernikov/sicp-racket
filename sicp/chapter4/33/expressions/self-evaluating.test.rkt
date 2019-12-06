#lang racket

(require rackunit)
(require "self-evaluating.rkt")

(check-true (self-evaluating? 5))
(check-true (self-evaluating? "5"))
(check-false (self-evaluating? car))
