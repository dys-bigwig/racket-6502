#lang racket
(require "ops.rkt"
         "utility.rkt"
         "parsack.rkt"
         "instruction.rkt"
         "context.rkt")
(require data/pvector
         data/collection)

(define (extract-label str)
  (substring str 0 (sub1 (string-length str))))

(define src (parse-file "source.asm"))

(define (first-pass parse-tree)
  (for/fold ([pc 0]
             [labels (hash)]
             #:result (Context 0 parse-tree labels (make-pvector pc 0)))
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

(define (emit-op name operand context)
  (define emit (get-in instructions
                       name
                       (Operand-mode operand)))
  (emit context))

(define (second-pass context)
  (let assemble ([context context])
    (match (Context-parse-tree context)
      ['() (Context-output context)]
      [(list (Instruction name operand) nodes ...)
       (assemble (emit-op name operand context))])))

(second-pass (first-pass src))
