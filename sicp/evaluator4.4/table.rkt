#lang racket

(require sicp)

(provide get put)

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
      (cond ((eq? m 'lookup) lookup)
            ((eq? m 'insert!) insert!)
            (else
             (error "Unknown operation: TABLE" m))))
    dispatch))

(define THE-TABLE (make-table))
(define get (THE-TABLE 'lookup))
(define put (THE-TABLE 'insert!))










