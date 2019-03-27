#lang sicp

(#%require rackunit)

(define (enumerate-interval from to)
  (if (> from to)
      nil
      (cons from (enumerate-interval (inc from) to))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (flatmap op seq)
  (accumulate append nil (map op seq)))

(define (filter predicate? seq)
  (cond ((null? seq) nil)
        ((predicate? (car seq)) (cons (car seq) (filter predicate? (cdr seq))))
        (else (filter predicate? (cdr seq)))))

(define empty-board nil)

(define (make-position row col)
  (list row col))
(define (position-row position)
  (car position))
(define (position-col position)
  (cadr position))

(define (safe? k position)
  (let ((current (car position))
        (others (cdr position)))
    (define (attack? q1 q2)
      (and (not (= (position-row q1) (position-row q2)))
           (not (= (abs (- (position-row q1) (position-row q2)))
                   (abs (- (position-col q1) (position-col q2)))))))
    (define (iter rest)
      (or (null? rest)
          (and (attack? current (car rest))
               (iter (cdr rest)))))
    (iter others)))

(define (adjoin-position new-row k rest-of-queens)
  (cons (make-position new-row k) rest-of-queens))

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (position) (safe? k position))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(check-equal? (queens 1) '(((1 1))))
(check-equal? (queens 3) '())
(check-equal? (queens 4) '(((3 4) (1 3) (4 2) (2 1)) ((2 4) (4 3) (1 2) (3 1))))
(check-equal? (length (queens 5)) 10)
(check-equal? (length (queens 6)) 4)
(check-equal? (length (queens 7)) 40)
(check-equal? (length (queens 8)) 92)
(check-equal? (length (queens 9)) 352)