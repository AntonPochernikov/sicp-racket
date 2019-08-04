#lang racket

; exchange procedure is implemented in terms of withdraw and deposit
; since this procedures are serialized, we won't be able to call them
; before exchange terminates
; at this point exchange will never complete
