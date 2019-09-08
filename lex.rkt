#lang racket
(require parser-tools/lex (prefix-in : parser-tools/lex-sre))
(require (for-syntax racket/match))
(require "number.rkt")
(require parser-tools/yacc)

(define-empty-tokens delimiters (lparen rparen lbracket dollar rbracket hashtag newline eof))
(define-empty-tokens directives (equate db))
(define-empty-tokens mnemonics (ADC AND ASL BCC BCS BEQ BIT BMI BNE BPL BRK BVC BVS CLC
                                    CLD CLI CLV CMP CPX CPY DEC DEX DEY EOR INC INX INY JMP
                                    JSR LDA LDX LDY LSR NOP ORA PHA PHP PLA PLP ROL ROR RTI
                                    RTS SBC SEC SED SEI STA STX STY TAX TAY TSX TXA TXS TYA))
(define-tokens atoms (8-bit-int 16-bit-int identifier string))

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

(define-lex-abbrev equate/l (:: (:or #\E #\e) (:or #\Q #\q) (:or #\U #\u)))
(define-lex-abbrev db/l (:: (:or #\D #\d) (:or #\B #\b)))
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
    [tokens mnemonics atoms delimiters]
    [start Line*]
    [end newline]
    [error (λ (tok-ok? name val)
              (error (format "~a ~a" name val)))]
    [grammar
      (Line*
        [(Line Line*) (cons $1 $2)]
        [() '()])
      (Line
        [(Opcode-Statement) $1])
      (Opcode-Statement
        [(Immediate-Opcode Immediate-Operand) (Opcode $1 $2)]
        [(Zero-Page-Opcode Zero-Page-Operand) (Opcode $1 $2)]
        [(Absolute-Opcode Absolute-Operand) (Opcode $1 $2)])
      (Immediate-Opcode
        [(ADC) 'ADC] [(AND) 'AND] [(CMP) 'CMP] [(CPX) 'CPX] [(CPY) 'CPY] [(EOR) 'EOR] [(LDA) 'LDA] [(LDX) 'LDX]
        [(LDY) 'LDY] [(ORA) 'ORA] [(PHA) 'PHA] [(PHP) 'PHP] [(SBC) 'SBC])
      (Zero-Page-Opcode
        [(ADC) 'ADC] [(AND) 'AND] [(ASL) 'ASL] [(BIT) 'BIT] [(CMP) 'CMP] [(CPX) 'CPX] [(CPY) 'CPY] [(DEC) 'DEC]
        [(EOR) 'EOR] [(INC) 'INC] [(LDA) 'LDA] [(LDX) 'LDX] [(LDY) 'LDY] [(LSR) 'LSR] [(ORA) 'ORA] [(ROL) 'ROL]
        [(ROR) 'ROR] [(SBC) 'SBC] [(STA) 'STA] [(STX) 'STX] [(STY) 'STY])
      (Zero-Page-X-Opcode
        [(ADC) 'ADC] [(AND) 'AND] [(ASL) 'ASL] [(CMP) 'CMP] [(DEC) 'DEC] [(EOR) 'EOR] [(INC) 'INC] [(LDA) 'LDA]
        [(LDY) 'LDY] [(LSR) 'LSR] [(ORA) 'ORA] [(ROL) 'ROL] [(ROR) 'ROR] [(SBC) 'SBC] [(STA) 'STA] [(STY) 'STY])
      (Absolute-Opcode
        [(LDY) 'LDY])
      (Int
        [(8-bit-int) $1]
        [(16-bit-int) $1])
      (Immediate-Operand
        [(hashtag Int) (Operand $2 'IMM)])
      (Zero-Page-Operand
        [(8-bit-int) (Operand $1 'ZP)])
      (Absolute-Operand
        [(16-bit-int) (Operand $1 'ABS)])]))

(define (lex+parse str)
  (define in (open-input-string str))
  (for/list ([line (in-producer (thunk (test-parse (thunk (test-lex in)))))]
             #:break (equal? line '()))
    (car line)))

(lex+parse "LDY $400")
