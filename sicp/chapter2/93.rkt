#lang sicp

(define (make-rational n d)
  (cons n d))

(define (make-polynomial variable term-list) (list variable term-list))
(define p1 (make-polynomial 'x '((2 1) (0 1))))
(define p2 (make-polynomial 'x '((3 1) (0 1))))
(define rf (make-rational p1 p2))
