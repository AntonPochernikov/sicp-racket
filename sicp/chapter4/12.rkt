#lang racket

(require sicp)

(define (make-environment . frames) frames)
(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

(define (make-frame variables values)
  (cons
   'frame-table
   (map cons variables values)))

(define (frame-pairs frame) (cdr frame))

(define (add-bindings-to-frame! variable value frame)
  (let ([pairs (frame-pairs frame)])
    (set! pairs
          (cons (cons variable value)
                pairs))))

(define (extend-environment vars vals base-env)
  (cond [(= (length vars) (length vals))
         (cons (make-frame vars vals) base-env)]
        [(< (length vars) (length vals))
         (error "Too many arguments supplied" vars vals)]
        [else
         (error "Too few arguments supplied" vars vals)]))

; SOLUTION
(define (traverse-environment env var on-find on-fail)
  (define (loop env)
    (if (eq? env the-empty-environment)
        (on-fail)
        (let* ([frame (first-frame env)]
               [ref (assoc var (frame-pairs frame))])
          (if ref
              (on-find ref)
              (enclosing-environment env)))))
  (loop env))

(define (lookup-variable-value var env)
  (traverse-environment env
                        var
                        cdr
                        (lambda ()
                          (error "Unbound variable" var))))

(define (set-variable-value! var val env)
  (traverse-environment env
                        var
                        (lambda (ref) (set-cdr! ref val))
                        (lambda ()
                          (error "Unbound variable" var))))

(define (define-variable! var val env)
  (let ([frame (first-frame env)])
    (traverse-environment (make-environment frame)
                        var
                        (lambda (ref) (set-cdr! ref val))
                        (lambda ()
                          (add-bindings-to-frame! var val frame)))))


















