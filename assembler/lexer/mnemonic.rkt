#lang racket/base
(require (only-in parser-tools/lex define-lex-abbrev))
(require (only-in "transformer.rkt" :strings-ci))
(provide mnemonic/l)
(define-lex-abbrev mnemonic/l (:strings-ci "ADC" "AND" "ASL" "BCC" "BCS" "BEQ" "BIT" "BMI" "BNE" "BPL" "BRK" "BVC" "BVS" "CLC"
                                           "CLD" "CLI" "CLV" "CMP" "CPX" "CPY" "DEC" "DEX" "DEY" "EOR" "INC" "INX" "INY" "JMP"
                                           "JSR" "LDA" "LDX" "LDY" "LSR" "NOP" "ORA" "PHA" "PHP" "PLA" "PLP" "ROL" "ROR" "RTI"
                                           "RTS" "SBC" "SEC" "SED" "SEI" "STA" "STX" "STY" "TAX" "TAY" "TSX" "TXA" "TXS" "TYA"))
