#lang racket
(require lens)
(require data/collection)
(require json fancy-app)
(require "utility.rkt")
(provide instruction-opcode instruction-length lookup)
(define hex-string->number (compose (string->number _ 16)
                                    (substring _ 1)))

(define (get-in h ks)
  (cond
    [(empty? ks) h]
    [else
      (let ([k (first ks)])
        (let ([h (hash-ref h k)])
          (get-in h (rest ks))))]))

;(define (one-byte-proc hex)
;  (λ ())) Just carry context around and let procs get args themselves to avoid sending superfluous value in 1-byte procs

(struct Context (pc parse-tree labels output))

(define (two-byte-proc hex)
  (λ (context)
     (match context
       [(struct* Context ([pc pc]
                          [parse-tree tree]
                          [labels labels]
                          [output output]))
        (define instruction ())
        (Context (+ pc 2)
                 (cdr parse-tree)
                 labels
                 (set-nth* output
                           pc hex
                           (+ pc 1) (Instruction-operand)))])
     (values (+ pc 2)
             (set-nth* output
                       pc hex
                       (+ pc 1) value))))

(define (three-byte-proc hex)
  (λ (value pc output)
     (values (+ pc 3)
             (set-nth* output
                       pc hex
                       (+ pc 1) (addr-hibyte value)
                       (+ pc 2) (addr-lobyte value)))))

(define (mode-string->symbol str)
  (case str
    [("Implied") '(IMP . #f)]
    [("Immediate") '(IMM . #f)]
    [("Absolute") '(ABS . #f)]
    [("Absolute,X") '(ABS . X)]
    [("Absolute,Y") '(ABS . Y)]
    [("ZeroPage") '(ZP . #f)]
    [("ZeroPage,X") '(ZP . X)]
    [("ZeroPage,Y") '(ZP . Y)]
    [("(Indirect,X)") '(IND . X)]
    [("(Indirect),Y") '(IND . Y)]))

(define instructions
  (for/fold ([result (hash)])
    ([op (read-json (open-input-file "6502_instructions.json"))])
    (match-define (hash-table ('opcode hex) ('name name) ('mode mode) ('bytes length)) op)
    (hash-update result
                 (string->symbol name)
                 (hash-set _ (mode-string->symbol mode) (cond
                                                          [(= (string->number length) 2) (two-byte-proc hex)]
                                                          [(= (string->number length) 3) (three-byte-proc hex)]))
                 hash)))

(pretty-print instructions)

(define (lookup name mode)
  (get-in instructions `(,name ,mode)))

(define (instruction-length name mode)
  (get-in instructions `(,name ,mode length)))

(define (instruction-opcode name mode)
  (get-in instructions `(,name ,mode hex)))

;(define (instruction-length name mode)
;  (case name
;    [(JMP) 3]
;    [(BEQ) 2]
;    [(BCC) 2]
;    [(BCS) 2]
;    [(BEQ) 2]
;    [(BMI) 2]
;    [(BNE) 2]
;    [(BPL) 2]
;    [(BVC) 2]
;    [(BVS) 2]
;    [else
;    (case mode
;      [(IMM) 2]
;      [(ZP) 2]
;      [(ZPX) 2]
;      [(ZPY) 2]
;      [(ABS) 3]
;      [(ABSX) 3]
;      [(ABSY) 3]
;      [(INDX) 2]
;      [(INDY) 2])]))
