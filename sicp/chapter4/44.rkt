#lang racket

(require sicp)

(define (every pred L)
  (cond [(null? L) true]
        [(not (pred (car L))) false]
        [else
         (every pred (cdr L))]))

(define (safe? q1 q2)
  (and (not (eq? (car q1) (car q2)))
       (not (eq? (cadr q1) (cadr q2)))
       (not (eq? (abs (- (car q1) (cadr q2)))
                 (abs (- (cadr q1) (cadr q2)))))))

(define (length L) (reduce inc 0 L))

(define (queens-puzzle n)
  (define (iter acc)
    (if (= n (length acc))
        acc
        (let ([queen (list (amb 1 2 3 4 5 6 7 8)
                           (amb 1 2 3 4 5 6 7 8))])
          (require
            (every (lambda (q)
                     (safe? queen q))
                   acc))
          (iter (cons queen acc)))))
  (iter '()))










