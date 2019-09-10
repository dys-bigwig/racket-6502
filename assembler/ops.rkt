#lang racket
(require lens)

(define (lookup mnemonic mode index)
  (lens-view (hash-ref-nested-lens mnemonic (cons mode index)) instructions))

(define hex-string->number (compose (string->number _ 16)
                                    (substring _ 1)))
(require json fancy-app)

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
