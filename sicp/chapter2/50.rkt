#lang sicp

(#%require sicp-pict)

(define (flip-horiz painter)
  (lambda (frame)
    ((transform-painter painter
                        (make-vect 1.0 1.0)
                        (make-vect 0.0 1.0)
                        (make-vect 1.0 0.0)) frame)))

(paint (flip-horiz einstein))

(define rotate180 flip-horiz)

(paint (rotate180 einstein))

(define (rotate270 painter)
  (lambda (frame)
    ((transform-painter painter
                        (make-vect 0.0 1.0)
                        (make-vect 0.0 0.0)
                        (make-vect 1.0 1.0)) frame)))

(paint (rotate270 einstein))
