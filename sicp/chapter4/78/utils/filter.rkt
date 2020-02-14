#lang racket

(require sicp)

(provide filter)

(define (filter pred L)
  (cond [(null? L) '()]
        [(pred (car L))
         (cons (car L)
               (filter pred (cdr L)))]
        [else
         (filter pred (cdr L))]))






