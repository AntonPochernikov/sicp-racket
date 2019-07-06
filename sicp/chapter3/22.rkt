#lang sicp

(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr '()))
    (define (queue) (cons front-ptr rear-ptr))
    (define (set-front-ptr! item) (set! front-ptr item))
    (define (set-rear-ptr! item) (set! rear-ptr item))
    (define (empty-queue?) (null? front-ptr))
    (define (front-queue queue)
      (if (empty-queue?)
          (error "Can not get head of empty queue" front-ptr)
          (car (front-ptr))))
    (define (insert-queue! item)
      (let ((new-pair (cons item '())))
        (cond ((empty-queue?)
               (set-front-ptr! new-pair)
               (set-rear-ptr! new-pair)
               (queue))
              (else
               (set-cdr! rear-ptr new-pair)
               (set-rear-ptr! new-pair)
               (queue)))))
    (define (delete-queue!)
      (cond ((empty-queue?)
             (error "DELETE! called with empty queue" (queue)))
            (else
             (set-front-ptr! (cdr front-ptr))
             (queue))))
    (define (print-queue)
      (display front-ptr))

    (define (dispatch m)
      (cond ((eq? m 'insert-queue!) insert-queue!)
            ((eq? m 'delete-queue!) (delete-queue!))
            ((eq? m 'print-queue) (print-queue))
            (else (error "Unknown message -- MAKE-QUEUE" m))))
    dispatch))

(define q1 (make-queue))
(q1 'print-queue)
((q1 'insert-queue!) 'a)
(q1 'print-queue)
((q1 'insert-queue!) 'b)
(q1 'print-queue)
(q1 'delete-queue!)
(q1 'print-queue)
(q1 'delete-queue!)
(q1 'print-queue)