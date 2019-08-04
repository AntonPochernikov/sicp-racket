#lang racket

(require sicp)

(define (test-and-set! cell)
  (if (car cell)
      true
      (begin (set-car! cell true)
             false)))

(define (clear! L)
  (set! L (list false)))

(define (make-mutex)
  (let [(cell (list false))]
    (define (the-mutex m)
      (cond [(eq? m 'acquire)
             (if (test-and-set! cell)
                 (the-mutex 'acquire))]
            [(eq? m 'release) (clear! cell)]
            [else
             (error "Wrong message: MAKE-MUTEX" m)]))
    the-mutex))

(define (make-serializer)
  (let [(mutex (make-mutex))]
    (lambda (p)
      (define (serialized-p . args)
        (mutex 'acquire)
        (let [(val (apply p args))]
          (mutex 'release)
          val))
      serialized-p)))

; SOLUTION

(define (make-mutex-list n)
  (if (= n 0)
      '()
      (cons (make-mutex)
            (make-mutex-list (- n 1)))))

(define (length L)
  (if (null? L)
      0
      (+ 1 (length (cdr L)))))

(define (take L index)
  (cond [(= index 0) false]
        [(null? L) false]
        [(= index 1) (car L)]
        [else (take (cdr L) (- index 1))]))

(define (make-semaphor n)
  (if (= n 0)
      (error "0 processes: MAKE-SEMAPHOR")
      (let [(mutex-list (make-mutex-list n))
            (current 1)]
        (define (next n)
          (if (= (length mutex-list) n)
              1
              (inc n)))
        (lambda (p)
          (define (serialized-p . args)
            (let [(mutex (take mutex-list current))]
              (set! current (next current))
              (mutex 'acquire)
              (let [(val (apply p args))]
                (mutex 'release)
                val)))
          serialized-p))))

(define (make-semaphor-atomic threshold)
  (if (= threshold 0)
      (error "0 processes: MAKE-SEMAPHOR-ATOMIC")
      (let [(processes 0)]
        (define (test-and-set!)
          (if (= processes threshold)
              true
              (begin (set! processes (inc processes))
                     false)))
        (define (the-semaphor m)
          (cond [(eq? m 'acquire)
                 (if (test-and-set!)
                     (the-semaphor 'acquire))]
                [(eq? m 'release)
                 (set! processes (- processes 1))]
                [else
                 (error "Wrong message: MAKE-SEMAPHOR-ATOMIC" m)]))
        the-semaphor)))
















