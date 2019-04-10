#lang sicp

(define (install-it-department)
  (define (make-record name salary position)
    (list name salary position))
  (define (name-record record)
    (car record))
  (define (salary-record record)
    (cadr record))
  (define (position-record record)
    (caddr record))

  (define (get-record file name)
    (if (null? file)
        #f
        (let ((current-record (car file))
              (rest (cdr file)))
          (if (eq? (name-record current-record) name)
              current-record
              (get-record rest name)))))
  (define (get-salary record)
    (salary-record record))

  (put 'get-record 'it get-record)
  (put 'get-salary 'it get-salary)
  'done)

(define (install-sales-department)
  (define (make-record name salary adress)
    (list name salary adress))
  (define (name-record record)
    (car record))
  (define (salary-record record)
    (cadr record))
  (define (adress-record record)
    (caddr record))

  (define (get-record file name)
    (if (null? file)
        #f
        (let ((current-record (car file))
              (rest (cdr file)))
          (if (eq? (name-record current-record) name)
              current-record
              (get-record rest name)))))
  (define (get-salary record)
    (salary-record record))

  (put 'get-record 'it get-record)
  (put 'get-salary 'it get-salary)
  'done)

(define (attach-tag tag content)
  (cons tag content))
(define (get-tag data) (car data))
(define (get-content data) (cdr data))

(define (get-record file employee)
  (attach-tag (get-department file)
              ((get 'get-record (get-department file)) file employee)))
(define (get-salary record)
  (let ((department (get-tag record))
        (content (get-content record)))
    ((get 'get-salary department) content)))

(define (find-employee-record employee file-list)
  (if (null? file-list)
      #f
      (let ((file (car file-list))
            (rest (cdr file-list)))
        (let ((record (get-record file employee)))
          (if record
              record
              (find-employee-record employee rest))))))

; new department could be installed by creating new installer


