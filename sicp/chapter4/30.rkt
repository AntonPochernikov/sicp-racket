#lang racket

; a)
; For-each generates separate call of procedure on
; every argument.

; We can even call for-each with another procedure:
; (lambda (x) (set! count (+ count 1)) (display x)) (list 57 321 88))

; Since display and set! are primitive procedures
; we will get our side effects correctly.


; b)

(define (p1 x) 
  (set! x (cons x '(2)))
  x)
(define (p2 x)
  (define (p e) e x)
  (p (set! x
           (cons x '(2)))))

; (p1 10) => '(10 2)
; first expression (set! x ...)
; will be evaluated and than x would be returned

; (p2 10) => 10
; Our (set! x ...) expression will be thunked
; as argument of compound procedure p and will not evaluated
; that will not produce actual value.


; c)

; Changing eval-sequence won't affect our program cause
; force-it procedure will just return object as the result if
; it is not a thunk or evaluated thunk.

; d)

; Cy`s approach is actually better for side effects in sequences,
; since it will produce expected behavior,
; when side effect is performed by procedure passed as argument.

; Nonetheless it won't change behavior we've seen in ex 4.27,
; so side effects in normal-order language are sort of unexpected.
; As the result i would prefer original implementation of eval-sequence,
; because it is more consistent with lazy evaluator idea.










