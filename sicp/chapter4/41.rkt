#lang racket

(require sicp)

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

(define (reduce iteratee init L)
  (if (null? L)
      init
      (reduce iteratee
              (iteratee init (car L))
              (cdr L))))

(define (flatmap proc L)
  (reduce (lambda (acc item)
            (append acc (proc item)))
          '()
          L))

(define (permutate L)
  (if (null? L)
      (list '())
      (flatmap
       (lambda (current)
         (map (lambda (combination)
                (cons current combination))
              (permutate (without current L))))
       L)))

(define (filter-map predicates L)
  (if (null? predicates)
      L
      (filter-map (cdr predicates)
                  (filter (car predicates) L))))

(display
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
   (lambda (names)
     (not (eq? (abs (- (ref 'smith names)
                       (ref 'fletcher names)))
               1)))
   (lambda (names)
     (not (eq? (abs (- (ref 'fletcher names)
                       (ref 'cooper names)))
               1))))
  (permutate names)))












