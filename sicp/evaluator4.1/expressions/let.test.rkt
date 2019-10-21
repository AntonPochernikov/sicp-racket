#lang racket

(require sicp)
(require rackunit)
(require "let.rkt")

(define test '(let ((x 5) (y 3))
                (> x y)))

(check-true (let? test))
(check-false (let? '()))

(check-equal? (let->combination test)
              '((lambda (x y)
                  (> x y))
                5
                3))

(define test2 '(let sum ((a 5) (b 2))
                 (+ a b)))

(check-equal? (let->combination test2)
              '((lambda ()
                  (define sum
                    (lambda (a b)
                      (+ a b)))
                  (sum 5 2))))

(define test* '(let* ((x 5) (y x))
                 y))

(check-true (let*? test*))
(check-false (let*? test))

(check-equal? (let*->nested-lets test*)
              '(let ((x 5))
                 (let ((y x))
                   y)))


(define testrec '(letrec
                     ((even? (lambda (n)
                               (if (= n 0) true (odd? (- n 1)))))
                      (odd? (lambda (n)
                              (if (= n 0) false (even? (- n 1))))))
                   10))


(check-true (letrec? testrec))
(check-false (letrec? test))

(check-equal?
 (letrec->let testrec)
 '(let ((even? *unassigned*)
        (odd? *unassigned*))
    (set! even? (lambda (n)
                  (if (= n 0) true (odd? (- n 1)))))
    (set! odd? (lambda (n)
                 (if (= n 0) false (even? (- n 1)))))
    10))








  
