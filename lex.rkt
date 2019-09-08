#lang racket
(require parser-tools/lex (prefix-in : parser-tools/lex-sre))
(require (for-syntax racket/match))
(require "number.rkt")
(require parser-tools/yacc)



(define-empty-tokens mnemonics (ADC AND ASL BCC BCS BEQ BIT BMI BNE BPL BRK BVC BVS CLC
                                    CLD CLI CLV CMP CPX CPY DEC DEX DEY EOR INC INX INY JMP
                                    JSR LDA LDX LDY LSR NOP ORA PHA PHP PLA PLP ROL ROR RTI
                                    RTS SBC SEC SED SEI STA STX STY TAX TAY TSX TXA TXS TYA))

(define-empty-tokens delimiters (lparen rparen lbracket dollar rbracket hashtag newline eof))
(define-empty-tokens directives (equate db))
(define-tokens opcodes (opcode))
(define-tokens atoms (8-bit-int 16-bit-int identifier string))
(define-empty-tokens indexes (comma-x comma-y))

(define-lex-abbrev identifier/l (:& (complement number/l)
                                    (complement opcode/l)
                                    (complement directive/l)
                                    (:+ (:or alphabetic numeric))))

(define-for-syntax (char-ci char)
  `(union ,(char-upcase char)
          ,(char-downcase char)))

(define-for-syntax (string-ci str)
  `(concatenation ,@(map char-ci (string->list str))))

(define-lex-trans :char-ci (λ (stx)
                             (match (syntax->datum stx)
                               [(list :char-ci char) (datum->syntax stx (char-ci char))])))

(define-lex-trans :string-ci (λ (stx)
                               (match (syntax->datum stx)
                                 [(list :string-ci str) (datum->syntax stx (string-ci str))])))

(define-lex-trans :strings-ci (λ (stx)
                                 (match (syntax->datum stx)
                                   [(list :strings-ci ss ..1) (datum->syntax stx `(union ,@(map string-ci ss)))])))

(define-lex-abbrev mnemonic/l (:strings-ci "ADC" "AND" "ASL" "BCC" "BCS" "BEQ" "BIT" "BMI" "BNE" "BPL" "BRK" "BVC" "BVS" "CLC"
                                           "CLD" "CLI" "CLV" "CMP" "CPX" "CPY" "DEC" "DEX" "DEY" "EOR" "INC" "INX" "INY" "JMP"
                                           "JSR" "LDA" "LDX" "LDY" "LSR" "NOP" "ORA" "PHA" "PHP" "PLA" "PLP" "ROL" "ROR" "RTI"
                                           "RTS" "SBC" "SEC" "SED" "SEI" "STA" "STX" "STY" "TAX" "TAY" "TSX" "TXA" "TXS" "TYA"))

(define-lex-abbrev equate/l (:: (:char-ci #\E) (:char-ci #\Q) (:char-ci #\U)))
(define-lex-abbrev db/l (:: (:char-ci #\D) (:char-ci #\B)))
(define-lex-abbrev directive/l (:or equate/l db/l))

(define (mnemonic->token m)
  (case m
    [("ADC") (token-ADC)] [("AND")  (token-AND)] [("ASL")  (token-ASL)] [("BCC")  (token-BCC)] [("BCS")  (token-BCS)]
    [("BEQ") (token-BEQ)] [("BIT")  (token-BIT)] [("BMI")  (token-BMI)] [("BNE")  (token-BNE)] [("BPL")  (token-BPL)]
    [("BRK") (token-BRK)] [("BVC")  (token-BVC)] [("BVS")  (token-BVS)] [("CLC")  (token-CLC)] [("CLD")  (token-CLD)]
    [("CLI") (token-CLI)] [("CLV")  (token-CLV)] [("CMP")  (token-CMP)] [("CPX")  (token-CPX)] [("CPY")  (token-CPY)] 
    [("DEC") (token-DEC)] [("DEX")  (token-DEX)] [("DEY")  (token-DEY)] [("EOR")  (token-EOR)] [("INC")  (token-INC)]
    [("INX") (token-INX)] [("INY")  (token-INY)] [("JMP")  (token-JMP)] [("JSR")  (token-JSR)] [("LDA")  (token-LDA)]
    [("LDX") (token-LDX)] [("LDY")  (token-LDY)] [("LSR")  (token-LSR)] [("NOP")  (token-NOP)] [("ORA")  (token-ORA)] 
    [("PHA") (token-PHA)] [("PHP")  (token-PHP)] [("PLA")  (token-PLA)] [("PLP")  (token-PLP)] [("ROL")  (token-ROL)]
    [("ROR") (token-ROR)] [("RTI")  (token-RTI)] [("RTS")  (token-RTS)] [("SBC")  (token-SBC)] [("SEC")  (token-SEC)]
    [("SED") (token-SED)] [("SEI")  (token-SEI)] [("STA")  (token-STA)] [("STX")  (token-STX)] [("STY")  (token-STY)]
    [("TAX") (token-TAX)] [("TAY")  (token-TAY)] [("TSX")  (token-TSX)] [("TXA")  (token-TXA)] [("TXS")  (token-TXS)]
    [("TYA") (token-TYA)]))

(define (string->int-token s #:base [base 10])
  (define int (string->number s base))
  ((if (> #xFF int) token-8-bit-int token-16-bit-int) int))

(define test-lex
  (lexer
    [(:+ (:or #\space #\tab)) (test-lex input-port)]
    [(:+ #\newline) (token-newline)]
    [#\# (token-hashtag)]
    [mnemonic/l (mnemonic->token lexeme)]
    [(:+ numeric) (string->int-token lexeme)]
    [(:: #\$ (:+ hex-digit)) (string->int-token (substring lexeme 1) #:base 16)]
    [(:: #\, (:char-ci #\X)) (token-comma-x)]
    [(:: #\, (:char-ci #\Y)) (token-comma-y)]
    [#\( (token-lparen)]
    [#\) (token-rparen)]
    [(eof) (token-newline)]))

(struct Opcode (name operand) #:transparent)
(struct Operand (val mode) #:transparent)

(define-syntax (Zero-Page-X-Opcode stx)
  (syntax-case stx ()
    [(_ ops ...)
     #`'(Zero-Page-X-Opcode
         #,@(for/list ([op (syntax->list #'(ops ...))])
              #`((#,op) (quote #,op))))]))

(define test-parse
  (parser
    [tokens mnemonics atoms delimiters indexes]
    [start Line*]
    [end newline]
    [precs (right comma-x)]
    [error (λ (tok-ok? name val)
              (error (format "~a ~a" name val)))]
    [debug "debug.txt"]
    [grammar
      (Line*
        [(Line Line*) (cons $1 $2)]
        [() '()])
      (Line
        [(Instruction) $1])
      (Instruction
        [(ADC ADC-Operand) (Opcode 'ADC $2)]
        ;[(AND AND-Operand) (Opcode $1 $2)]
        ;[(ASL ASL-Operand) (Opcode $1 $2)]
        ;[(BCC BCC-Operand) (Opcode $1 $2)]
        ;[(BCS BCS-Operand) (Opcode $1 $2)]
        ;[(BEQ BEQ-Operand) (Opcode $1 $2)]
        ;[(BIT BIT-Operand) (Opcode $1 $2)]
        ;[(BMI BMI-Operand) (Opcode $1 $2)]
        ;[(BNE BNE-Operand) (Opcode $1 $2)]
        ;[(BPL BPL-Operand) (Opcode $1 $2)]
        ;[(BRK BRK-Operand) (Opcode $1 $2)]
        ;[(BVC BVC-Operand) (Opcode $1 $2)]
        ;[(BVS BVS-Operand) (Opcode $1 $2)]
        ;[(CLC CLC-Operand) (Opcode $1 $2)]
        ;[(CLD CLD-Operand) (Opcode $1 $2)]
        ;[(CLI CLI-Operand) (Opcode $1 $2)]
        ;[(CLV CLV-Operand) (Opcode $1 $2)]
        ;[(CMP CMP-Operand) (Opcode $1 $2)]
        ;[(CPX CPX-Operand) (Opcode $1 $2)]
        ;[(CPY CPY-Operand) (Opcode $1 $2)]
        ;[(DEC DEC-Operand) (Opcode $1 $2)]
        ;[(DEX DEX-Operand) (Opcode $1 $2)]
        ;[(DEY DEY-Operand) (Opcode $1 $2)]
        ;[(EOR EOR-Operand) (Opcode $1 $2)]
        ;[(INC INC-Operand) (Opcode $1 $2)]
        ;[(INX INX-Operand) (Opcode $1 $2)]
        ;[(INY INY-Operand) (Opcode $1 $2)]
        ;[(JMP JMP-Operand) (Opcode $1 $2)]
        ;[(JSR JSR-Operand) (Opcode $1 $2)]
        ;[(LDA LDA-Operand) (Opcode $1 $2)]
        ;[(LDX LDX-Operand) (Opcode $1 $2)]
        ;[(LDY LDY-Operand) (Opcode $1 $2)]
        ;[(LSR LSR-Operand) (Opcode $1 $2)]
        ;[(NOP NOP-Operand) (Opcode $1 $2)]
        ;[(ORA ORA-Operand) (Opcode $1 $2)]
        ;[(PHA PHA-Operand) (Opcode $1 $2)]
        ;[(PHP PHP-Operand) (Opcode $1 $2)]
        ;[(PLA PLA-Operand) (Opcode $1 $2)]
        ;[(PLP PLP-Operand) (Opcode $1 $2)]
        ;[(ROL ROL-Operand) (Opcode $1 $2)]
        ;[(ROR ROR-Operand) (Opcode $1 $2)]
        ;[(RTI RTI-Operand) (Opcode $1 $2)]
        ;[(RTS RTS-Operand) (Opcode $1 $2)]
        ;[(SBC SBC-Operand) (Opcode $1 $2)]
        ;[(SEC SEC-Operand) (Opcode $1 $2)]
        ;[(SED SED-Operand) (Opcode $1 $2)]
        ;[(SEI SEI-Operand) (Opcode $1 $2)]
        ;[(STA STA-Operand) (Opcode $1 $2)]
        ;[(STX STX-Operand) (Opcode $1 $2)]
        ;[(STY STY-Operand) (Opcode $1 $2)]
        ;[(TAX TAX-Operand) (Opcode $1 $2)]
        ;[(TAY TAY-Operand) (Opcode $1 $2)]
        ;[(TSX TSX-Operand) (Opcode $1 $2)]
        ;[(TXA TXA-Operand) (Opcode $1 $2)]
        ;[(TXS TXS-Operand) (Opcode $1 $2)]
        ;[(TYA TYA-Operand) (Opcode $1 $2)]
        )
      (ADC-Operand
        [(IMM-Operand) $1]
        [(ZP-Operand) $1]
        [(ZPX-Operand) $1]
        [(ZPY-Operand) $1]
        [(ABS-Operand) $1]
        [(ABSX-Operand) $1]
        [(ABSY-Operand) $1]
        [(INDX-Operand) $1]
        [(INDY-Operand) $1])
      (IMM-Operand
        [(hashtag 8-bit-int) (Operand $2 'IMM)]
        [(hashtag 16-bit-int) (Operand $2 'IMM)])
      (ZP-Operand
        [(8-bit-int) (Operand $1 'ZP)])
      (ZPX-Operand
        [(8-bit-int comma-x) (Operand $1 'ZPX)])
      (ZPY-Operand
        [(8-bit-int comma-y) (Operand $1 'ZPY)])
      (ABS-Operand
        [(16-bit-int) (Operand $1 'ABS)])
      (ABSX-Operand
        [(16-bit-int comma-x) (Operand $1 'ABSX)])
      (ABSY-Operand
        [(16-bit-int comma-y) (Operand $1 'ABSY)])
      (INDX-Operand
        [(lparen 8-bit-int comma-x rparen) (Operand $2 'INDX)])
      (INDY-Operand
        [(lparen 8-bit-int comma-y rparen) (Operand $2 'INDY)])]))

(define (lex+parse str)
  (define in (open-input-string str))
  (for/list ([line (in-producer (thunk (test-parse (thunk (test-lex in)))))]
             #:break (equal? line '()))
    (car line)))

(lex+parse "ADC ($42,x)")
