#lang racket

(require (only-in "table.rkt" put get))
(require rackunit)

(check-false (get 'key1 'key2))

(define test '())
(put 'key1 'key2 test)
(check-equal? (get 'key1 'key2) test)









