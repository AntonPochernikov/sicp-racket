#lang racket

(require sicp)

; SOLUTION
; I've had to work it out without amb for now
; and the answer is 5

(define (filter pred L)
  (cond [(null? L) L]
        [(pred (car L))
         (cons (car L) (filter pred (cdr L)))]
        [else
         (filter pred (cdr L))]))

(define (without item L)
  (filter (lambda (x) (not (eq? x item)))
          L))

(define (ref item L)
  (cond [(null? L) '()]
        [(eq? item (car L)) 1]
        [else
         (+ 1 (ref item (cdr L)))]))

(define names '(baker cooper fletcher miller smith))

(define (permutate L)
  (define (iter current rest)
    (let ([rest-permutations
           (map (lambda (comb)
                  (cons current comb))
                (permutate (without current L)))])
      (if (null? rest)
          rest-permutations
          (append rest-permutations
                  (iter (car rest)
                        (cdr rest))))))
  (if (or (null? L) (null? (cdr L)))
      (list L)
      (iter (car L) (cdr L))))

(define (filter-map predicates L)
  (if (null? predicates)
      L
      (filter-map (cdr predicates)
                  (filter (car predicates) L))))

(define (length L)
  (if (null? L)
      0
      (+ 1 (length (cdr L)))))

(length
 (filter-map
  (list
   (lambda (names)
     (not (eq? (ref 'baker names) 5)))
   (lambda (names)
     (not (eq? (ref 'cooper names) 1)))
   (lambda (names)
     (not (eq? (ref 'fletcher names) 5)))
   (lambda (names)
     (not (eq? (ref 'fletcher names) 1)))
   (lambda (names)
     (> (ref 'miller names)
        (ref 'cooper names)))
   ;(lambda (names)
   ;  (not (eq? (abs (- (ref 'smith names)
   ;                    (ref 'fletcher names)))
   ;            1)))
   (lambda (names)
     (not (eq? (abs (- (ref 'fletcher names)
                       (ref 'cooper names)))
               1))))
  (permutate names)))









