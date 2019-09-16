#lang racket
(require "ops.rkt"
         "utility.rkt"
         "parsack.rkt"
         "instruction.rkt")
(require data/pvector
         data/collection)

(define src (parse-file "source.asm"))

(define (resolve-labels parse-tree)
  (for/fold
    ([pc 0] [labels (hash)]
     #:result pc)
    ([node parse-tree])
    (match node
      [(struct* Instruction ([name name]
                             [operand operand]))
       (values (instruction-length name
                                   (cons (Operand-mode operand)
                                         (Operand-index operand)))
               labels)]

      [(struct* Label ([name name]))
       (values pc
               (hash-set labels
                         name pc))]

      [_ (values pc labels)])))

(define (in-pcs parse-tree)
  (reverse (for/fold ([pc '(0)])
            ([node parse-tree])
    (match node
      [(struct* Instruction ([name name]
                             [operand operand]))
       (cons (+ (instruction-length name (cons (Operand-mode operand)
                                            (Operand-index operand)))
                (car pc))
             pc)]
      [_ (cons (car pc) pc)]))))

(define (assemble parse-tree labels)
  (reverse
    (for/fold ([output '()])
              ([node parse-tree]
               [pc (in-pcs parse-tree)])
      (match node
        [(struct* Instruction ([name name]
                               [operand operand]))
         (cons (cons pc
                     (lookup name
                             (cons (Operand-mode operand)
                                   (Operand-index operand))))
               output)]
        [_ output]))))

(assemble src (resolve-labels src))

;(define (emit-instruction instruction)
;  (define operand (get-operand instruction))
;  (define-values (val mode) (values (get-operand-val operand)
;                                    (get-operand-mode operand))) (write-byte))
;
;(define (test parse-tree)
;  (for ([node parse-tree])
;    (with-output-to-file "test.bin" #:exists 'replace
;     (λ ()
;        (match node
;          [(? Instruction?) 
;           (write-byte (char->integer #\A))])))))
;
;(struct Context (src labels))
