#lang sicp

(#%require rackunit)

(define (make-table same-key?)
  (let ((local-table (list '*table*)))
    (define (assoc key records)
      (cond ((null? records) false)
            ((same-key? key (caar records)) (car records))
            (else (assoc key (cdr records)))))
    (define (lookup keys)
      (define (iter table rest-keys)
        (cond ((null? rest-keys) false)
              ((null? (cdr rest-keys))
               (let ((record (assoc (car rest-keys) (cdr table))))
                 (if record
                     (cdr record)
                     false)))
              (else
               (let ((subtable (assoc (car rest-keys) (cdr table))))
                 (if subtable
                     (iter subtable (cdr rest-keys))
                     false)))))
      (iter local-table keys))
    (define (insert! keys value)
      (define (iter table rest-keys)
        (cond ((null? rest-keys)
               (error "Expected list with at least one key as an argument -- TABLE" rest-keys))
              ((null? (cdr rest-keys))
               (let ((record (assoc (car rest-keys) (cdr table))))
                 (if record
                     (set-cdr! record value)
                     (set-cdr! table
                               (cons (cons (car rest-keys) value)
                                     (cdr table))))))
              (else
               (let ((subtable (assoc (car rest-keys) (cdr table))))
                 (if subtable
                     (iter subtable (cdr rest-keys))
                     (begin
                       (set-cdr! table
                                 (cons (list (car rest-keys)
                                             (cons (cadr rest-keys) nil))
                                       (cdr local-table)))
                       (iter (cadr table) (cdr rest-keys))))))))
      (iter local-table keys))
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define t (make-table eq?))
(define get (t 'lookup-proc))
(define put (t 'insert-proc!))

(put '(1) 'value-1)
(put '(2 1) 'value-2)
(put '(3 1 1) 'value-3)
(put '(4 1 1 1) 'value-4)

(check-equal? (get '(1)) 'value-1)
(check-equal? (get '(2 1)) 'value-2)
(check-equal? (get '(3 1 1)) 'value-3)
(check-equal? (get '(4 1 1 1)) 'value-4)
