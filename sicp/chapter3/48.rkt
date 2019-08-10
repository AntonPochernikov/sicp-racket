#lang racket

; this method will avoid deadlock cause every operation with simiral accounts
; will start from account with smallest id

(define (exchange account1 account2)
  (let [(difference (- (account1 'balance)
                       (account2 'balance)))]
    ((account1 'withdraw) difference)
    ((account2 'deposit) difference)))

(define (serialized-exchange account1 account2)
  (let [(id1 (account1 'id))
        (id2 (account2 'id))
        (serializer1 (account1 'serializer))
        (serializer2 (account2 'serializer))]
    (cond [(> id1 id2)
           ((serializer2 (serializer1 exchange)) account1 account2)]
          [(< id1 id2)
           ((serializer1 (serializer2 exchange)) account1 account2)]
          [else
           (error "Trying to exchange account with itself" account1)])))

(define (use-id p)
  (define counter 0)
  (define (inner balance)
    (set! counter (inc counter))
    (p balance counter))
  inner)

(define (make-account-and-serializer balance id)
  (define (get-balance) balance)
  (define (get-id) id)
  (define (withdraw amount)
    (if (> amount balance)
        "Inceficient funds"
        (begin (set! balance (- balance amount))
               balance)))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)

  (let [(balance-serializer (make-serializer))]
    (define (dispatch m)
      (cond [(eq? m 'balance) get-balance]
            [(eq? m 'id) get-id]
            [(eq? m 'withdraw) withdraw]
            [(eq? m 'deposit) deposit]
            [(eq? m 'serializer) balance-serializer]
            [else
             (error "Unknown message: MAKE-ACCOUNT-AND-SERIALIZER" m)]))
    dispatch))

(define (make-account balance)
  ((use-id make-account-and-serializer) balance))














