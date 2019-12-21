#lang racket

(require sicp)

(define *unparsed* '())

; SOLUTION
(define (list-to-amb L)
  (if (null? L)
      (amb)
      (amb (car L) (list-to-amb (cdr L)))))

(define (parse-word word-list)
  (require (not (null? *unparsed*)))
  (require (memq (car *unparsed*) (cdr word-list)))
  (set! *unparsed* (cdr *unparsed*))
  (list-to-amb (cdr word-list)))
















