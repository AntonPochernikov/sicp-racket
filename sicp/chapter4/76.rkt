#lang racket

(require sicp)

; still doesn't work correctly with "unique" and "not"

(define (failed? exp)
  (eq? 'failed exp))

(define (merge-frames f1 f2)
  (cond [(or (failed? f1) (failed? f2)) 'failed]
        [(null? f1) f2]
        [else
         (let* ([binding (car f1)]
                [variable (binding-variable binding)]
                [value (binding-value binding)]
                [extended-f2 (extend-if-possible variable value f2)])
           (if (failed? extended-f2)
               'failed
               (merge-frames (cdr f1) extended-f2)))]))

(define (merge-conjuncts s1 s2)
  (stream-flatmap
   (lambda (frame1)
     (stream-filter
      (lambda (f) (not (failed? f)))
      (stream-map
       (lambda (frame2)
         (merge-frames frame1 frame2))
       s2)))
   s1))

(define (conjoin conjuncts frame-stream)
  (if (empty-conjunction? conjuncts)
      frame-stream
      (merge-conjuncts
       (qeval (first-conjunct conjuncts) frame-stream)
       (conjoin (rest-conjuncts conjuncts) frame-stream))))






