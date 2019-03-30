#lang racket

(require ( planet "sicp.ss" ( "soegaard" "sicp.plt" 2 1)))

(define wave-segments (list (make-segment (make-vect 0.0 0.8) (make-vect 0.2 0.6))
                            (make-segment (make-vect 0.2 0.6) (make-vect 0.3 0.65))
                            (make-segment (make-vect 0.3 0.65) (make-vect 0.35 0.65))
                            (make-segment (make-vect 0.35 0.65) (make-vect 0.3 0.8))
                            (make-segment (make-vect 0.3 0.8) (make-vect 0.35 1))
                            
                            (make-segment (make-vect 0.4 0.75) (make-vect 0.5 0.7))
                            (make-segment (make-vect 0.5 0.7) (make-vect 0.6 0.75))
                            
                            (make-segment (make-vect 0.65 1) (make-vect 0.7 0.8))
                            (make-segment (make-vect 0.7 0.8) (make-vect 0.65 0.65))
                            (make-segment (make-vect 0.65 0.65) (make-vect 0.7 0.65))
                            (make-segment (make-vect 0.7 0.65) (make-vect 1 0.35))
                            (make-segment (make-vect 1 0.2) (make-vect 0.7 0.45))
                            (make-segment (make-vect 0.7 0.45) (make-vect 0.75 0))
                            (make-segment (make-vect 0.6 0) (make-vect 0.5 0.3))
                            (make-segment (make-vect 0.5 0.3) (make-vect 0.4 0))
                            (make-segment (make-vect 0.25 0) (make-vect 0.3 0.45))
                            (make-segment (make-vect 0.3 0.45) (make-vect 0.2 0.4))
                            (make-segment (make-vect 0.2 0.4) (make-vect 0 0.65))))
(define wave (segments->painter wave-segments))

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))
(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left up)
              (bottom-right right)
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))

(paint (corner-split wave 4))

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

(define (square-limit painter n)
  (let ((combine4 (square-of-four flip-vert (compose flip-vert flip-horiz)
                                  identity flip-horiz)))
    (combine4 (corner-split painter n))))

(paint (square-limit wave 4))
