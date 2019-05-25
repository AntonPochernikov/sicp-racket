#lang sicp

(#%require rackunit)

(define (make-account balance pass)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Not enough funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch p m)
    (cond ((not (eq? p pass)) (lambda (a) "Incorrect password -- MAKE-ACCOUNT"))
          ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request -- MAKE-ACCOUNT"
                       m))))
  dispatch)

(define peter-acc (make-account 100 'open-sesame))
(check-equal? ((peter-acc 'open-sesame 'withdraw) 40) 60)
(check-equal? ((peter-acc 'some-other-password 'deposit) 50) "Incorrect password -- MAKE-ACCOUNT")

; solution
(define (make-joint acc pass joint-pass)
  (lambda (p m)
    (if (eq? p joint-pass)
        (acc pass m)
        (lambda (a) "Incorrect password -- MAKE-JOINT"))))

(define paul-acc
  (make-joint peter-acc 'open-sesame 'rosebud))

(check-equal? ((paul-acc 'rosebud 'deposit) 50) 110)
(check-equal? ((peter-acc 'open-sesame 'deposit) 50) 160)
(check-equal? ((paul-acc 'open-sesame 'withdraw) 50) "Incorrect password -- MAKE-JOINT")
