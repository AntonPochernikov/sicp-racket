#lang racket

(require sicp)

; SOLUTION
; I don't have access to amb-evaluator
; so i solved it with filtered permutations.

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

(define (find pred L)
  (cond [(null? L) false]
        [(pred (car L)) (car L)]
        [else
         (find pred (cdr L))]))

(define (mega-map iteratee . lists)
  (if (null? (car lists))
      '()
      (cons (apply iteratee (map car lists))
            (apply mega-map
                   (append (list iteratee) (map cdr lists))))))

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

(define daughters '(gabrielle melissa rosalind mary-ann lorna))
; note that fathers and yachts lists are related by index
(define fathers '(moore colonel-downing hall barnacle-hood parker))
(define yachts '(lorna melissa rosalind gabrielle mary-ann))

(define (make-relation father daughter yacht) (list father daughter yacht))
(define (father relation) (car relation))
(define (daughter relation) (cadr relation))
(define (yacht relation) (caddr relation))

(display
 (filter-map
  (list (lambda (solution)
          (find (lambda (relation)
                  (and (eq? (daughter relation) 'melissa)
                       (eq? (father relation) 'barnacle-hood)))
                solution))
        (lambda (solution)
          (find (lambda (relation)
                  (and (eq? (daughter relation) 'mary-ann)
                       (eq? (father relation) 'moore)))
                solution))
        (lambda (solution)
          (not
           (find (lambda (relation)
                   (and (eq? (daughter relation) 'gabrielle)
                        (eq? (father relation) 'parker)))
                 solution)))
        (lambda (solution)
          (not
           (find (lambda (relation)
                   (and (eq? (daughter relation) 'rosalind)
                        (eq? (father relation) 'hall)))
                 solution)))
        (lambda (solution)
          (let ([gabrielles-father-yacht
                 (yacht
                  (find (lambda (relation)
                          (eq? (daughter relation) 'gabrielle))
                        solution))]
                [parkers-daughter
                 (daughter
                  (find (lambda (relation)
                          (eq? (father relation) 'parker))
                        solution))])
            (eq? gabrielles-father-yacht parkers-daughter))))
  (map (lambda (combination-of-daughters)
         (mega-map make-relation
                   fathers
                   combination-of-daughters
                   yachts))
       (permutate daughters))))

; We have one solution
; (((moore mary-ann lorna)
;   (colonel-downing lorna melissa)
;   (hall gabrielle rosalind)
;   (barnacle-hood melissa gabrielle)
;   (parker rosalind mary-ann)))
; where Lorna is Colonel Downing daughter.

; There are 3 solutions to this puzzle if we do not know
; that Mary Ann's father is Mr. Moore.

; Here is nondeterminictis solution.
; I'm not sure it's most the efficient one, but it is what it is...
(define (distinct? items)
  (cond [(null? items) true]
        [(null? (cdr items)) true]
        [(member (car items) (cdr items)) false]
        [else (distinct? (cdr items))]))

(define (yacht-puzzle)
  (let ([moore-rel
         (make-relation
          'moore
          (amb 'gabrielle 'melissa 'rosalind 'mary-ann 'lorna)
          (amb 'lorna 'melissa 'rosalind 'gabrielle 'mary-ann))])
    (require (eq? (daughter moore-rel) 'mary-ann))
    (require (eq? (yacht moore-rel) 'lorna))
    (let ([barnacle-hood-rel
           (make-relation
            'barnacle-hood
            (amb 'gabrielle 'melissa 'rosalind 'mary-ann 'lorna)
            (amb 'lorna 'melissa 'rosalind 'gabrielle 'mary-ann))])
      (require (eq? (daughter barnacle-hood-rel) 'melissa))
      (require (eq? (yacht barnacle-hood-rel) 'gabrielle))
      (let ([hall-rel
             (make-relation
              'hall
              (amb 'gabrielle 'melissa 'rosalind 'mary-ann 'lorna)
              (amb 'lorna 'melissa 'rosalind 'gabrielle 'mary-ann))])
        (require (eq? (yacht hall-rel) 'rosalind))
        (let ([colonel-downing-rel
               (make-relation
                'colonel-downing
                (amb 'gabrielle 'melissa 'rosalind 'mary-ann 'lorna)
                (amb 'lorna 'melissa 'rosalind 'gabrielle 'mary-ann))])
          (require (eq? (yacht colonel-downing-rel) 'melissa))
          (let ([parker-rel
                 (make-relation
                  'parker
                  (amb 'gabrielle 'melissa 'rosalind 'mary-ann 'lorna)
                  (amb 'lorna 'melissa 'rosalind 'gabrielle 'mary-ann))])
            (require (distinct? (mega-map daughter
                                          moore-rel
                                          barnacle-hood-rel
                                          hall-rel
                                          colonel-downing-rel
                                          parker-rel)))
            (require (distinct? (mega-map yacht
                                          moore-rel
                                          barnacle-hood-rel
                                          hall-rel
                                          colonel-downing-rel
                                          parker-rel)))
            (require (eq? (daughter parker-rel)
                          (yacht
                           (find (lambda (rel)
                                   (eq? (daughter rel) 'gabrielle))
                                 (list moore-rel
                                       barnacle-hood-rel
                                       hall-rel
                                       colonel-downing-rel
                                       parker-rel)))))))))))












