#lang racket
(require lens)
(provide lookup-length)
(require json fancy-app)
(define hex-string->number (compose (string->number _ 16)
                                    (substring _ 1)))

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
                 (hash-set _ (mode-string->symbol mode) (hash 'hex (hex-string->number hex)
                                                         'length (string->number length)))
                 hash)))

(define (lookup name mode)
  (lens-view (hash-ref-nested-lens name mode) instructions))


(define (lookup-length name mode)
  (case name
    [(JMP) 3]
    [(BEQ) 2]
    [(BCC) 2]
    [(BCS) 2]
    [(BEQ) 2]
    [(BMI) 2]
    [(BNE) 2]
    [(BPL) 2]
    [(BVC) 2]
    [(BVS) 2]
    [else (hash-ref (lookup name mode) 'length)]))
