#lang sicp

(#%require sicp-pict)

(define (below painter1 painter2)
  (rotate90 (beside (rotate270 painter2) (rotate270 painter1))))

(paint (below einstein mark-of-zorro))
