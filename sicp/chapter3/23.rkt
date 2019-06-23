#lang sicp

(#%require rackunit)

(define (front-ptr deque) (car deque))
(define (rear-ptr deque) (cdr deque))
(define (set-front-ptr! deque item) (set-car! deque item))
(define (set-rear-ptr! deque item) (set-cdr! deque item))
(define (empty-deque? deque) (null? (front-ptr deque)))

(define (make-deque) (cons '() '()))

(define (front-deque deque)
  (if (empty-deque? deque)
      (error "expected not-empty deque -- FRONT-DEQUE" deque)
      (car (front-ptr deque))))

(define (rear-deque deque)
  (if (empty-deque? deque)
      (error "expected not-empty deque -- REAR-DEQUE" deque)
      (car (rear-ptr deque))))

(define (front-insert-deque! deque item)
  (let ((new-pair (cons (cons item nil) nil)))
    (cond ((empty-deque? deque)
           (set-front-ptr! deque new-pair)
           (set-rear-ptr! deque new-pair))
          (else
           (set-cdr! new-pair (front-ptr deque))
           (set-cdr! (car (front-ptr deque)) new-pair)
           (set-front-ptr! deque new-pair)))))

(define (rear-insert-deque! deque item)
  (let ((new-pair (cons (cons item nil) nil)))
    (cond ((empty-deque? deque)
           (set-front-ptr! deque new-pair)
           (set-rear-ptr! deque new-pair))
          (else
           (set-cdr! (rear-ptr deque) new-pair)
           (set-cdr! (car new-pair) (rear-ptr deque))
           (set-rear-ptr! deque new-pair)))))

(define (rear-delete-deque! deque)
  (cond ((empty-deque? deque)
         (error "Can not remove items from empty deque -- REAR-DELETE-DEQUE!" deque))
        (else (set-rear-ptr! deque (cdar (rear-ptr deque)))
              (if (null? (rear-ptr deque))
                  (set-front-ptr! deque nil)
                  (set-cdr! (rear-ptr deque) nil)))))

(define (front-delete-deque! deque)
  (cond ((empty-deque? deque)
         (error "Can not remove items from empty deque -- REAR-DELETE-DEQUE!" deque))
        (else (set-front-ptr! deque (cdr (front-ptr deque)))
              (if (null? (front-ptr deque))
                  (set-rear-ptr! deque nil)
                  (set-cdr! (car (front-ptr deque)) nil)))))

(define (includes L item)
  (cond ((null? L) #f)
        ((eq? (car L) item) #t)
        (else (includes (cdr L) item))))

(define (get-deck-list deque)
  (define (iter passed acc rest)
    (cond ((null? rest) acc)
          ((includes passed rest) acc)
          (else (iter (append passed rest)
                      (cons (caar rest) acc)
                      (cdar rest)))))
  (iter '() '() (rear-ptr deque)))

(define (print-deque deque)
  (newline)
  (display (get-deck-list deque)))

(define d1 (make-deque))
(check-equal? (get-deck-list d1) '())
(rear-insert-deque! d1 1)
(check-equal? (get-deck-list d1) '(1))
(rear-insert-deque! d1 2)
(rear-insert-deque! d1 3)
(check-equal? (get-deck-list d1) '(1 2 3))

(front-insert-deque! d1 0)
(rear-insert-deque! d1 4)
(check-equal? (get-deck-list d1) '(0 1 2 3 4))

(rear-delete-deque! d1)
(rear-delete-deque! d1)
(check-equal? (get-deck-list d1) '(0 1 2))

(front-delete-deque! d1)
(front-delete-deque! d1)
(check-equal? (get-deck-list d1) '(2))
(front-delete-deque! d1)
(check-equal? (get-deck-list d1) '())
