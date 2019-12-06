#lang racket

(require sicp)

; SOLUTION
(define (quoted? exp) (tagged-list? exp 'quote))
(define (text-of-quotation exp env)
  (define (quoted-list->cons exp)
    (cond [(null? exp) ''()]
          [(pair? exp)
           (list 'cons
                 (quoted-list->cons (car exp))
                 (quoted-list->cons (cdr exp)))]
          [else `',exp]))
  (let ([text (cadr exp)])
    (if (not (pair? text))
        text
        (eval (quoted-list->cons text) env))))







