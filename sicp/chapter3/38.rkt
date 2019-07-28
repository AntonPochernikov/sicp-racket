#lang racket

; balance = 100

; Peter => (set! balance (+ balance 10))
; Paul => (set! balance (- balance 20))
; Mary => (set! balance (- balance (/ balance 2)))

; a)
; Peter 110, Paul 90, Mary 45
; Peter 110, Mary 55, Paul 35
; Paul 80, Peter 90, Mary 45
; Paul 80, Mary 40, Peter 50
; Mary 50, Peter 60, Paul 40
; Mary 50, Paul 40, Peter 60


; b)
; Peter and Paul access balance at them same time (100)
; Peter sets balance to 110
; Mary access balance (110)
; Paul sets balance to 80
; Mary sets balance to 55
