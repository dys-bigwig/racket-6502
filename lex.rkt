#lang racket
(require parser-tools/lex (prefix-in : parser-tools/lex-sre))
(require (for-syntax racket/match))
(require "number.rkt")
(require parser-tools/yacc)

(define-empty-tokens delimiters (lparen rparen lbracket dollar rbracket hashtag comma newline eof))
(define-empty-tokens directives (equate db))
(define-tokens mnemonics (mnemonic))
(define-tokens atoms (8-bit-int 16-bit-int identifier string))
(define-empty-tokens letters (A X Y))

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

(define (string->int-token s #:base [base 10])
  (define int (string->number s base))
  ((if (> #xFF int) token-8-bit-int token-16-bit-int) int))

(define test-lex
  (lexer
    [(:+ (:or #\space #\tab)) (test-lex input-port)]
    [(:+ #\newline) (token-newline)]
    [#\# (token-hashtag)]
    [mnemonic/l (token-mnemonic lexeme)]
    [(:+ numeric) (string->int-token lexeme)]
    [(:: #\$ (:+ hex-digit)) (string->int-token (substring lexeme 1) #:base 16)]
    [#\( (token-lparen)]
    [#\) (token-rparen)]
    [#\, (token-comma)]
    [(:char-ci #\A) (token-A)]
    [(:char-ci #\X) (token-X)]
    [(:char-ci #\Y) (token-Y)]
    [(eof) (token-newline)]))

(struct Opcode (name operand) #:transparent)
(struct Operand (val mode index) #:transparent)

(define test-parse
  (parser
    [tokens mnemonics atoms delimiters letters]
    [start Line*]
    [end newline]
    [error (λ (tok-ok? name val)
              (error (format "~a ~a" name val)))]
    [debug "debug.txt"]
    [grammar
      (Line*
        [(Line Line*) (cons $1 $2)]
        [() '()])
      (Line
        [(mnemonic Operand?) (Opcode $1 $2)])
      (Int
        [(8-bit-int) $1]
        [(16-bit-int) $1])
      (Operand?
        [(hashtag Int) $2]
        [(8-bit-int Index?) (Operand $1 'ZP $2)]
        [(16-bit-int Index?) (Operand $1 'ABS $2)]
        [(lparen 8-bit-int comma X rparen) (Operand $2 'IND 'X)]
        [(lparen 8-bit-int rparen comma Y) (Operand $2 'IND 'Y)]
        [() (Operand #f 'IMP #f)]
        [(A) 'A])
      (Index?
        [(X) 'X]
        [(Y) 'Y]
        [() #f]) ]))

(define (lex+parse str)
  (define in (open-input-string str))
  (for/list ([line (in-producer (thunk (test-parse (thunk (test-lex in)))))]
             #:break (equal? line '()))
    (car line)))

(lex+parse "ADC ($42,x)")
