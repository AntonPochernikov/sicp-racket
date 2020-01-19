#lang racket

(require sicp)

(define (singleton-stream? s)
  (and (not (stream-null? s))
       (stream-null? (stream-cdr s))))

; unique
(define (unique-query exp) (car exp))
(define (uniquely-asserted contents frame-stream)
  (stream-flatmap
   (lambda (frame)
     (let ([result-stream
            (qeval (unique-query contents)
                   (singleton-stream frame))])
       (if (singleton-stream? result-stream)
           result-stream
           the-empty-stream)))
   frame-stream))
(put 'unique 'qeval uniquely-asserted)

(and (supervisor ?x ?y) (unique (supervisor ?anyone ?y)))









