#lang racket

(require sicp)

(provide the-empty-environment
         extend-environment
         lookup-variable-value
         set-variable-value!
         define-variable!
         unbound-variable!
         unbound-variable)

(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

(define (make-frame variables values)
  (cons
   'frame-table
   (map cons variables values)))

(define (frame-pairs frame) (cdr frame))
(define (set-frame-pairs! frame pairs) (set-cdr! frame pairs))

(define (add-bindings-to-frame! variable value frame)
  (set-frame-pairs! frame
                    (cons (cons variable value)
                          (frame-pairs frame))))

(define (extend-environment vars vals base-env)
  (cond [(= (length vars) (length vals))
         (cons (make-frame vars vals) base-env)]
        [(< (length vars) (length vals))
         (error "Too many arguments supplied" vars vals)]
        [else
         (error "Too few arguments supplied" vars vals)]))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ([ref (assoc var (frame-pairs (first-frame env)))])
          (if ref
              (cdr ref)
              (env-loop (enclosing-environment env))))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ([ref (assoc var (frame-pairs (first-frame env)))])
          (if ref
              (set-cdr! ref val)
              (env-loop (enclosing-environment env))))))
  (env-loop env))

(define (define-variable! var val env)
  (let* ([frame (first-frame env)]
         [ref (assoc var (frame-pairs frame))])
    (if ref
        (set-cdr! ref val)
        (add-bindings-to-frame! var val frame))))

(define (unbound-variable exp) (cadr exp))

(define (filter pred L)
      (cond [(null? L) '()]
            [(pred (car L))
             (cons (car L)
                   (filter pred (cdr L)))]
            [else
             (filter pred (cdr L))]))

(define (unbound-variable! var env)
  (let* ([frame (first-frame env)]
         [pairs (frame-pairs frame)])
    (set-frame-pairs!
     frame
     (filter (lambda (p)
               (not (eq? (car p) var)))
             pairs))))









