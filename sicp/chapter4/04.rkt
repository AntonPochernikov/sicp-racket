#lang racket

(require sicp)

; SOLUTION
(define (and-expressions exp) (cdr exp))
(define (or-expressions exp) (cdr exp))

(define (first-exp exps) (car exp))
(define (rest-exp exps) (cdr exp))
(define (empty-exp? exps) (null? exps))
(define (last-exp? exps) (null? (cdr exps)))

(define (eval-and exp env)
  (define (iter exps)
    (cond [(empty-exp? exps) 'true]
          [(last-exp? exps)
           (let ([first (first-exp exps)])
             (if (true? (eval first env))
                 first
                 'false))]
          [(true? (eval (first-exp exps) env))
           (iter (rest-exps exps))]
          [else 'false]))
  (iter (and-expressions exp)))

(define (eval-or exp env)
  (define (iter exps)
    (cond [(empty-exp? exps) 'false]
          [(true? (eval (first-exp exps) env))
           (first-exp exps)]
          [else
           (iter (rest-exps exp))]))
  (iter (and-expressions exp))


; (put 'eval 'and eval-and)
; (put 'eval 'or eval-or)


; DERIVED EXPRESSIONS
(define (true? exp) (eq? exp true))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))
(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))
(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

(define (eval-and-2 exp env)
  (let ([exps (and-expressions exp)])
    (eval (and->if exp) env)))

(define (and->if exps)
  (if (empty-exp? exps)
      'true
      (let ([first (first-exp exps)]
            [rest (rest-exps exps)])
        (if (empty-exp? rest)
            (make-if first
                     first
                     'false)
            (make-if first
                     (and->if (rest-exps exps))
                     'false)))))

(define (eval-or-2 exp env)
  (let ([exps (or-expressions exp)])
    (eval (or->if exp) env)))

(define (or->if exps)
  (if (empty-exp? exps)
      'false
      (let ([first (first-exp exps)]
            [rest (rest-exps exps)])
        (make-if (true? first)
                 first
                 (or->if rest)))))










