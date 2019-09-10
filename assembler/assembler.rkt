#lang racket
(require "parser/parse.rkt"
         "ops.rkt"
         "utility.rkt")

(define src (lex+parse "source.asm"))

(define (resolve-labels parse-tree)
  (for/fold
    ([pc 0] [labels (hash)])
    ([node parse-tree])
    (match node
      [(? Instruction? instruction) (define name (get-instruction-name instruction))
                                    (define operand (get-operand instruction))
                                    (define value (get-operand-value operand))
                                    (define mode (get-operand-mode operand))
                                    (define instruction-length (lookup-length name mode))
                                    (values (+ pc instruction-length)
                                            labels)]
      [(? Label? label) (values pc
                                (hash-set labels
                                          (get-label-name label) pc))]
      [_ (values pc labels)])))

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
(struct Context (src labels))

(resolve-labels src)
