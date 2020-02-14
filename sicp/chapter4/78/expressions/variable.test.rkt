#lang racket

(require rackunit)
(require "variable.rkt")

(check-true (variable? 'false))
(check-true (variable? (quote ok)))
(check-false (variable? +))
