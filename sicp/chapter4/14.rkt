#lang racket

(require sicp)

; SOLUTION
; Can't make it that way cause then we will try
; to apply our procedure, that is actually a (list 'compound <args> <body>).
; Thus we are mixing up operations of interpreted language and driven language.

(define (map p L)
  (if (null? L)
      '()
      (cons (p (car L))
            (map p (cdr L)))))

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list 'list list)
        (list '* *)
        (list '+ +)
        (list 'map map)))















