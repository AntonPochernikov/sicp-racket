#lang sicp

; before putting selectors for complex type apply-generic is called with 'complex type
; at that point we don`t have proper operation in list

; after putting this procedure in the list apply-generic is calling magnitude with 'complex type
; this procedure is calling magnitude with content of complex number
; next magnitude is calling apply-generic with current number type and we are getting proper selector
