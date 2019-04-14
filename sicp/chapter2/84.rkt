#lang sicp

(define (integer->rational x)
  (make-rational x 1))
(put-coersion 'integer 'rational integer->rational)

(define (rational->real x)
  (make-real (/ (numer x) (denom x))))
(put-coersion 'rational 'real rational->real)

(define (real->complex x)
  (make-complex-from-real-imag x 0))
(put-coersion 'real 'complex real->complex)

(define type-tower '(integer rational real complex))

(define (raise x)
  (let ((type (type-tag x)))
    (define (try-tower tower)
      (if (null? tower)
          (error "can not raise this type" x type-tower)
          (let ((current (car tower))
                (next (cadr tower))
                (rest (cdr tower)))
            (if (eq? current type)
                (if next
                    ((get-coersion type next) x)
                    (error "can not raise this type" x type-tower))
                (try-tower rest)))))
    (try-tower tower)))

(define (get-highest x y)
  (let ((type1 (type-tag x))
        (type2 (type-tag y)))
    (define (iter types)
      (if (null? types)
          false
          (let ((current (car types))
                (rest (cdr types)))
            (cond ((eq? current type1) x)
                  ((eq? current type2) y)
                  (else (iter (cdr types)))))))
    (iter type-tower)))

(define (same-type tags)
  (cond ((< (length tags) 2) true)
        ((not (eq? (car tags) (cadr tags))) false)
        (else (same-type (cdr tags)))))

(define (are-different-types x y)
  (not (eq? (type-tag x) (type-tag y))))

(define (find-different-types args)
  (define (iter acc rest)
    (if (< (length rest) 2)
        false
        (let ((prev (car rest))
              (next (cadr rest)))
          (if (are-different-types prev next)
              '(acc '(prev next) rest)
              (iter (cons acc prev) (cdr rest))))))
  (iter '() args))

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
                          (different (cadr coersible))
                          (rest (cadr coersible)))
                      (let ((arg1 (car different))
                            (arg2 (cadr different)))
                        (let ((highest (get-highest arg1 arg2)))
                          (cond ((eq? arg1 highest) (apply apply-generic (append op static (raise arg1) arg2 rest)))
                                ((eq? arg2 highest) (apply apply-generic (append op static arg1 (raise arg2) rest)))
                                (else (error "tying to compare types from different type towers -- APPLY-GENERIC" op args)))))))))))))

