#lang racket

; After several failed attempts to invent complex mechanism with promises
; described in the book, I've found hint in the internet
; about achiving this behavior with much less effort.
; All we have to do is rearranging complex queries so that
; filtering forms are always in the end.

; Thus we have to modify qeval procedure

; SOLUTION
(define (qeval query frame-stream)
  (let ([qproc (get (type query) 'qeval)])
    (if qproc
        (qproc (sort-queries (contents query))
               frame-stream)
        (simple-query query frame-stream))))

(define (simple-query query-pattern frame-stream)
  (stream-flatmap
   (lambda (frame)
     (stream-append-delayed
      (find-assertions query-pattern frame)
      (delay (apply-rules query-pattern frame))))
   frame-stream))

(define (partition pred? L)
  (define (iter truthy falsy rest)
    (cond [(null? rest) (cons truthy falsy)]
          [(pred? (car rest))
           (iter (cons (car rest) truthy)
                 falsy
                 (cdr rest))]
          [else
           (iter truthy
                 (cons (car rest) falsy)
                 (cdr rest))]))
  (iter '() '() L))

(define (sort-queries queries)
  (let ([result (partition filtering? queries)])
    (append (cdr result) (car result))))

(define (find proc L)
  (cond [(null? L) false]
        [(proc (car L))
         (car L)]
        [else (find proc (cdr L))]))

(define (filtering? query)
  (and (get (type query) 'qeval)
       (find (lambda (q)
               (eq? q (type query)))
             '(negate lisp-value))))














