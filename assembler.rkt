#lang racket
(require "ops.rkt"
         "utility.rkt"
         "parsack.rkt"
         "instruction.rkt")
(require data/pvector
         data/collection)

(define (extract-label str)
  (substring str 0 (sub1 (string-length str))))

(define src (parse-file "source.asm"))

(define (first-pass parse-tree)
  (for/fold ([pc 0]
             [labels (hash)]
             #:result (values pc labels))
            ([node parse-tree])
    (match node
      [(struct* Instruction ([name name]
                             [operand operand]))
       (values (+ pc (instruction-length name (Operand-mode operand)))
               labels)]

      [(struct* Label ([name name]))
       (values pc
               (hash-set labels (extract-label name) pc))]
      [(struct* Db ([bytes bs]))
       (values (+ pc (length bs))
               labels)])))

(define op-hash
  (hash 'LDA (hash '(IMM . #f) (λ (operand-value pc output)
                                  (values (+ pc 2)
                                          (set-nth* output
                                                    pc #xA9
                                                    (+ pc 1) operand-value))))))

(define (emit-op name operand pc output)
  ((hash-ref (hash-ref op-hash
                      name)
            (Operand-mode operand)) (Operand-value operand) pc output))

(define (test parse-tree size)
  (for/fold ([pc 0]
             [output (pvector size 0)]
             #:result output)
            ([node parse-tree])
    (match node
      [(struct* Instruction ([name name]
                             [operand operand]))
       (emit-op name operand pc output)])))

(test src 2)

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
