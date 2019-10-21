#lang racket

(require sicp)

(define (make-environment . frames) frames)
(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

(define (make-frame variables values)
  (cons
   'frame-table
   (map cons variables values)))

(define (frame-pairs frame) (cdr frame))

; SOLUTION
(define (unbound-variable exp) (cadr exp))
(define (make-unbound var)
  (list 'make-unbound! var))
(define (unbound! exp env)
  (let* ([frame (first-frame env)]
         [pairs (frame-pairs frame)]
         [var (unbound-variable exp)])
    (filter (lambda (p) (not (eq? (car p) var)))
            pairs)))

(put 'eval 'make-unbound! unbound!)


















