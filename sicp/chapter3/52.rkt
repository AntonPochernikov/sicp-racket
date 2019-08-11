#lang racket

(require sicp)

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (display-line x)
  (newline)
  (display x))

(define (display-stream s)
  (stream-for-each display-line s))

(define (stream-car s) (car s))
(define (stream-cdr s) (force (cdr s)))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream low
                   (stream-enumerate-interval (inc low) high))))

(define (stream-ref s n)
  (cond [(= n 0) (stream-car s)]
        [(stream-null? s) (error "No such ref in the stream: STREAM-REF" s n)]
        [else
         (stream-ref (stream-cdr s)
                     (- n 1))]))

(define (stream-filter pred s)
  (cond [(stream-null? s) the-empty-stream]
        [(pred (stream-car s))
         (cons-stream (stream-car s)
                      (stream-filter pred
                                     (stream-cdr s)))]
        [else (stream-filter pred
                             (stream-cdr s))]))

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))

; SOLUTION
(define sum 0)

(define (accum x)
  (set! sum (+ x sum))
  sum)

(define seq (stream-map accum (stream-enumerate-interval 1 20)))
; i will show what will be accum for every number in interval
;   1 2 3 4  5  6  7  8  9  10 11 12 13 14  15  16  17  18  19  20
;   - - - -  -  -  -  -  -  -  -- -  -- --- --- --- --- --- --- ---
;   1 3 6 10 15 21 28 36 45 55 66 78 91 105 120 136 153 171 190 210

; after definition of seq: (accum 1) => sum = 1

(define y (stream-filter even? seq))
; after definition of y:
; 1, (accum 2) => sum = 3, (accum 3) => sum = 6
; 6 is first even value of seq

(define z (stream-filter
           (lambda (x) (= (remainder x 5) 0))
           seq))

; after definition of z;
; 1, 3, 6, (accum 4) => sum = 10
; 10 is first value that's aliquot to 5

(stream-ref y 7)
; we have to evaluate 8th number in y sequence
; i will show all of them
;   1 2  3  4  5  6   7   8   9  10
;   - -- -- -- -- -- --- --- --- ---
;   6 10 28 36 66 78 120 136 190 210

; now sum is 136

(display-stream z)
; we will eval all seq
; i will show all values for z
; 1  2  3  4   5   6   7   8
; -- -- -- -- --- --- --- ---
; 10 15 45 66 105 120 190 210

; this values will be displayed
; sum = 210


; considering we had implemented delay as non-memoized procedure
; crutial thing is that we will eval (accum x) on every walk even
; if we`ve already evaluated that

; defining seq will set sum to 1, same as result above
; defining y will do this:
; (accum 2) => sum = 3
; (accum 3) => sum = 6

; defining z
; (accum 2) => sum = 8, (accum 3) => sum = 11; (accum 4) => sum = 15

; (stream-ref y 7)
; accum starts from 4
; sum is 6

;   0 => 6,
;   (accum 4) => sum = 19,
;   (accum 5) => sum = 24 (1),
;   (accum 6) => sum = 30 (2),
;   (accum 7) => sum = 37,
;   (accum 8) => sum = 45,
;   (accum 9) => sum = 54 (3),
;   (accum 10) => sum = 64 (4),
;   (accum 11) => sum = 75,
;   (accum 12) => sum = 87,
;   (accum 13) => sum = 100 (5),
;   (accum 14) => sum = 114 (6),
;   (accum 15) => sum = 129,
;   (accum 16) => sum = 145,
;   (accum 17) => sum = 162 (7),

; (display-stream z)
; accum starts from 5
; sum is 162

;  0  5   6   7   8   9   10  11  12  13  14  15  16  17  18  19  20
; -- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
; 15 167 173 180 188 197 207 218 230 243 257 272 288 305 323 342 362
; items displayed (15 180 230 305)


















