#lang racket

; serializing balance procedure is unnecessary,
; since this operation doesn't change state of the account
; we might as well see that this procedure will return valid result
; no matter what time it will be called
