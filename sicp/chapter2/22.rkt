#lang sicp

(#%require rackunit)

(define (square x) (* x x))

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things))
                    answer))))
  (iter items nil))

(display "result list will be reversed cause of cons arguments position")
(newline)
(square-list '(1 2 3 4))
(newline)

(define (square-list-2 items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items nil))

(display "changing arguments position will return a list with wrong structure")
(newline)
(square-list-2 '(1 2 3 4))
(newline)

(define (square-list-3 items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things))
                    answer))))
  (reverse (iter items nil)))

(display "we can get this work correctly by using 'reverse' procedure from exercise 2.18")
(newline)
(square-list-3 '(1 2 3 4))
(newline)
