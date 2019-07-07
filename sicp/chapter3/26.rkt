#lang sicp

(#%require rackunit)

; RECORDS

; TABLE
(define (make-table same-key?)
  (define (new-table) (list '*table*))
  (let ((local-table (new-table)))
    (define (name table) (car table))
    (define (get-records table) (cdr table))
    (define (set-records! table records) (set-cdr! table records))

    (define (make-records entry left right)
      (list entry left right))
    (define (entry records) (car records))
    (define (left-branch records) (cadr records))
    (define (right-branch records) (caddr records))

    (define (lockup-records given-key set-of-records)
      (define key car)
      (define value cdr)
      (cond ((null? set-of-records) false)
            ((equal? given-key (key (entry set-of-records)))
             (value (entry set-of-records)))
            ((< given-key (key (entry set-of-records)))
             (lockup-records given-key(left-branch set-of-records)))
            ((> given-key (key (entry set-of-records)))
             (lockup-records given-key (right-branch set-of-records)))))

    (define (adjoin-record x records)
      (define key car)
      (define value cdr)
      (cond ((null? records) (make-records x '() '()))
            ((equal? (key x) (key (entry records))) records)
            ((< (key x) (key (entry records)))
             (make-records (entry records)
                           (adjoin-record x (left-branch records))
                           (right-branch records)))
            ((> (key x) (key (entry records)))
             (make-records (entry records)
                           (left-branch records)
                           (adjoin-record x (right-branch records))))))
    (define (lookup keys)
      (define (iter table rest-keys)
        (cond ((null? rest-keys) false)
              ((not (pair? table)) false)
              ((null? (cdr rest-keys))
               (lockup-records (car rest-keys)
                               (get-records table)))
              ((not (pair? table)) false)
              (else
               (let ((subtable
                      (lockup-records (car rest-keys)
                                      (get-records table))))
                 (if subtable
                     (iter subtable (cdr rest-keys))
                     false)))))
      (iter local-table keys))

    (define (insert! keys value)
      (define (iter table rest-keys)
        (cond ((null? rest-keys)
               (error "Expected list with at least one key as an argument -- TABLE" rest-keys))
              ((null? (cdr rest-keys))
               (let ((record
                      (lockup-records (car rest-keys)
                                      (get-records table))))
                 (if record
                     (set! record value)
                     (set-records! table
                                   (adjoin-record (cons (car rest-keys) value)
                                                  (get-records table))))))
              (else
               (let ((subtable
                      (lockup-records (car rest-keys)
                                      (get-records table))))
                 (if subtable
                     (iter subtable (cdr rest-keys))
                     (let ((new-subtable (new-table)))
                       (set-records! table
                                     (adjoin-record (cons (car rest-keys)
                                                          new-subtable)
                                                    (get-records table)))
                       (iter new-subtable (cdr rest-keys))))))))
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
