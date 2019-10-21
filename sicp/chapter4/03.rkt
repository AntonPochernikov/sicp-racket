#lang racket

(require sicp)

; ==============================================
; TABLE
; ==============================================
(define (assoc key records)
  (cond [(null? records) false]
        [(eq? (caar records) key) (car records)]
        [else
         (assoc key (cdr records))]))

(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1 (cons key-2 value))
                            (cdr local-table))))) 'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else
             (error "Unknown operation: TABLE" m))))
    dispatch))

(define operations (make-table))
(define get (operations 'lookup-proc))
(define put (operations 'insert-proc!))

; ==============================================
; SOLUTION
; ==============================================
(put 'eval 'quote (lambda (exp env) (test-of-quotation exp)))
(put 'eval 'set! eval-assignment)
(put 'eval 'define eval-definition)
(put 'eval 'if eval-if)
(put 'eval 'lambda (lambda (exp env)
                     (make-procedure (lambda-parameters exp)
                                     (lambda-body exp)
                                     env)))
(put 'eval 'begin (lambda (exp env)
                    (eval-sequence (begin-actions exp)
                                   env)))
(put 'eval 'cond (lambda (exp env)
                   (eval (cond->if exp) env)))

(define (type exp) (car exp))
(define (contents exp) (cdr exp))

(define (eval exp env)
  (cond [(self-evaluating? exp) exp]
        [(variable? exp) (lookup-variable-value exp env)]
        ; dispatch on type
        [(get 'eval (type exp))
         ((get eval (type exp)) exp env)]
        [(application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env))]
        [else
         (error "Unknown expression type: EVAL" exp)]))


















