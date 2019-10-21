#lang racket

(require sicp)
(require rackunit)

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

; LAMBDA
(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

; LET
(define (make-let clauses body)
  (cons 'let (cons clauses body)))

; DEFINITIONS
(define (make-definition var val)
  (list 'define var val))

(define (definition? exp)
  (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp) ; formal parameters
                   (cddr exp)))) ; body

; SORT FUNCTION
(define (filter pred L)
  (cond [(null? L) '()]
        [(pred (car L))
         (cons (car L)
               (filter pred (cdr L)))]
        [else
         (filter pred (cdr L))]))

(define (sort compare L)
  (cond [(null? L) L]
        [(= (length L) 1) L]
        [else
         (let ([a (car L)]
               [rest (cdr L)])
           (let ([left (filter (lambda (x)
                                 (compare a x))
                               rest)]
                 [right (filter (lambda (x)
                                  (not (compare a x)))
                                rest)])
             (append (sort compare left)
                     (cons a (sort compare right)))))]))

; SEARCH TREE 
(define (some pred L)
  (cond [(null? L) false]
        [(pred (car L)) true]
        [else (some pred (cdr L))]))
; will search through nested listd to find symbol
(define (search-tree sym tree)
  (if (null? tree)
      false
      (some (lambda (node)
              (cond [(pair? node)
                     (search-tree sym node)]
                    [(eq? node sym) true]
                    [else false]))
            tree)))
; predicate shows that first variable contains second in its definition
(define (contains-term-in-def? container term)
  (let ([body (definition-value container)]
        [var (definition-variable term)])
    (search-tree var body)))

(define (scan-out-defines body)
  (let ([definitions (filter definition? body)]
        [rest-body (filter (lambda (exp)
                        (not (definition? exp)))
                      body)])
    (if (null? definitions)
        body
        (let ([vars (map definition-variable definitions)]
              [vals (map definition-value definitions)])
          (make-let (map (lambda (var)
                           (list var '*unassigned*))
                         vars)
                    ; sort assignments by predicate
                    (append
                     (sort
                      contains-term-in-def?
                      (map (lambda (var val)
                            (list 'set! var val))
                          vars
                          vals))
                     rest-body))))))

(check-equal?
 (scan-out-defines
  '((define b (+ a x))
    (define a 5)
    (+ a b)))
 '(let ((b *unassigned*)
        (a *unassigned*))
    (set! a 5)
    (set! b (+ a x))
    (+ a b)))

(check-equal?
 (scan-out-defines
  '((define c (+ a (b)))
    (define (b) a)
    (define a 5)
    (+ c a)))
 '(let ((c *unassigned*)
        (b *unassigned*)
        (a *unassigned*))
    (set! a 5)
    (set! b (lambda () a))
    (set! c (+ a (b)))
    (+ c a)))
 















