#lang sicp

(define (exists? label label-table)
  (assoc label label-table))

(define (extract-labels text on-receive)
  (if (null? text)
      (on-receive '() '())
      (extract-labels
       (cdr text)
       (lambda (instructions labels)
         (let ([next-intruction (car text)])
           (if (symbol? next-instruction)
               (if (exists? next-instruction labels)
                   (error "label already exists: EXTRACT-LABELS"
                          label)
                   (on-receive instructions
                               (cons (make-label-entry
                                      next-instruction
                                      instructions)
                                     labels)))
               (on-receive (cons (make-instruction
                                  next-instruction)
                                 instructions)
                           labels)))))))







