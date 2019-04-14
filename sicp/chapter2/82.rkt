#lang sicp

(define (same-type tags)
  (cond ((< (length tags) 2) true)
        ((not (eq? (car tags) (cadr tags))) false)
        (else (same-type (cdr tags)))))

(define (find-coercion tags)
  (define (iter acc rest)
    (if (< (length rest) 2)
        false
        (let ((prev (car rest))
              (next (cadr rest)))
          (if (or (get-coercion prev next)
                  (get-coercion next prev))
              '(acc '(prev next) rest)
              (iter (append acc prev) (cdr rest))))))
  (iter '() tags))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (same-type tags)
              (error "method not found" (list op type-tags))
              (let ((coersible (find-coercion tags)))
                (if coersible
                    (let ((static (car coersible))
                          (to-process (cadr coersible))
                          (rest (cadr coersible)))
                      (let ((type1 (car to-process))
                            (type2 (cadr to-process)))
                        (let ((t1->t2 (get-coercion type1 type2))
                              (t2->t1 (get-coercion type2 type1)))
                          (if t1->t2
                              (apply apply-generic (append op static (t1->t2 type1) type2 rest))
                              (apply apply-generic (append op static type1 (t2->t1 type2) rest))))))
                    (error "method not found, can not find proper type coercion" (list op type-tags)))))))))


; i build a procedure that is coercing arguments other way than listed in the book
; in this apply-generic we are consistently trying to coerce pairs of arguments from pair to pair
; we are also calling apply-generic with coerced arguments and the rest so that we`ll not miss proper generic
