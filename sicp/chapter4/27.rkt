#lang racket

(require sicp)

(define count 0)
(define (id x) (set! count (+ count 1)) x)

(define w (id (id 10)))

; after calling (id (id 10))
; procedure id will evaluate (set! count (+ count 1))
; return value (id 1) will be represented as a thunk

; so count is 1 at that moment

; after we lookup for w value this thunk will be evaluated giving
; it will call procedure id with 10 as argument,
; that will increment count again and return 10

; this time count will be 2
