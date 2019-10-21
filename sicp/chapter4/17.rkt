#lang racket

(require sicp)

; Extra frame appears when let is transformed to lambda expression.
; Lambda will be evaluated as another procedure
; that will produce a new frame when called.

; It is safe because let definitions won't affect outer environments

; We can also move definitions to the top of procedure body
; so that we can access any defined variable

(define (make-procedure parameters body env)
  (list 'procedure
        parameters
        (scan-out-defines body)
        env))

(define (scan-out-defines body)
  (let ([definitions (filter definition? body)]
        [rest-body (filter (lambda (exp)
                        (not (definition? exp)))
                      body)])
    (append definitions rest-body)))










