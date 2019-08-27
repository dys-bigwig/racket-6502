#lang racket
(require fancy-app)
(require "src.rkt")

#| general |#

(define skip-char read-char)

#| whitespace |#

(define (expect-char expected pred?)
  (define c (peek-char))
  (unless (pred? c)
    (error (format "expected: ~s\nreceived ~s" expected (peek-char)))))

(define (expect-str expected pred? len)
  (define s (peek-string len 0))
  (unless (pred? s)
    (error "expected: ~s\nreceived: ~s" expected s)))

(define whitespace?
  (memv _ (string->list "\t ")))

(define newline?
  (char=? _ #\newline))

(define (skip-whitespace)
  (when (whitespace? (peek-char))
    (skip-char)
    (skip-whitespace)))

(define (read-whitespace)
  (expect-char '#\space whitespace?)
  (read-char))

(define (read-whitespace+)
  (read-whitespace) 
  (skip-whitespace))

#| comments |#

(define (skip-comment)
  (when (char=? #\; (peek-char))
    (for ([c (in-port peek-char)]
          #:break (char=? c #\newline))
      (read-char))))

#| numbers |#

(define decimal-digit?
  (memv _ (string->list "0123456789")))

(define hex-digit?
  (disjoin decimal-digit? (memv _ (string->list "abcdefABCDEF"))))

(define string-numeric?
  string->number)

(define (read-decimal-digit)
  (expect-char 'decimal-digit decimal-digit?)
  (read-char))

(define (read-decimal-digit*)
  (if (decimal-digit? (peek-char))
    (cons (read-decimal-digit) (read-decimal-digit*))
    '()))

(define (read-decimal-digit+)
  (cons (read-decimal-digit)
        (read-decimal-digit*)))

(define (read-hex-digit)
  (expect-char 'hex-digit hex-digit?)
  (read-char))

(define (read-hex-digit*)
  (if (hex-digit? (peek-char))
    (cons (read-hex-digit) (read-hex-digit*))
    '()))

(define (read-hex-digit+)
  (cons (read-hex-digit)
        (read-hex-digit*)))

(define hex-string->number
  (compose (string->number _ 16) list->string))

(define decimal-string->number
  (compose (string->number _) list->string))

(define (parse-number)
  (cond
    [(char=? (peek-char) #\$) (skip-char)
                              (hex-string->number (read-hex-digit+))]
    [else (decimal-string->number (read-decimal-digit+))]))

#| opcodes |#

(struct OP-EXPR-TOKEN (opcode operand) #:transparent)
(struct OPCODE-TOKEN ())
(struct ADC OPCODE-TOKEN ()) (struct AND OPCODE-TOKEN ()) (struct ASL OPCODE-TOKEN ()) (struct BCC OPCODE-TOKEN ())
(struct BCS OPCODE-TOKEN ()) (struct BEQ OPCODE-TOKEN ()) (struct BIT OPCODE-TOKEN ()) (struct BMI OPCODE-TOKEN ())
(struct BNE OPCODE-TOKEN ()) (struct BPL OPCODE-TOKEN ()) (struct BRK OPCODE-TOKEN ()) (struct BVC OPCODE-TOKEN ())
(struct BVS OPCODE-TOKEN ()) (struct CLC OPCODE-TOKEN ()) (struct CLD OPCODE-TOKEN ()) (struct CLI OPCODE-TOKEN ())
(struct CLV OPCODE-TOKEN ()) (struct CMP OPCODE-TOKEN ()) (struct CPX OPCODE-TOKEN ()) (struct CPY OPCODE-TOKEN ())
(struct DEC OPCODE-TOKEN ()) (struct DEX OPCODE-TOKEN ()) (struct DEY OPCODE-TOKEN ()) (struct EOR OPCODE-TOKEN ())
(struct INC OPCODE-TOKEN ()) (struct INX OPCODE-TOKEN ()) (struct INY OPCODE-TOKEN ()) (struct JMP OPCODE-TOKEN ())
(struct JSR OPCODE-TOKEN ()) (struct LDA OPCODE-TOKEN ()) (struct LDX OPCODE-TOKEN ()) (struct LDY OPCODE-TOKEN ())
(struct LSR OPCODE-TOKEN ()) (struct NOP OPCODE-TOKEN ()) (struct ORA OPCODE-TOKEN ()) (struct PHA OPCODE-TOKEN ())
(struct PHP OPCODE-TOKEN ()) (struct PLA OPCODE-TOKEN ()) (struct PLP OPCODE-TOKEN ()) (struct ROL OPCODE-TOKEN ())
(struct ROR OPCODE-TOKEN ()) (struct RTI OPCODE-TOKEN ()) (struct RTS OPCODE-TOKEN ()) (struct SBC OPCODE-TOKEN ())
(struct SEC OPCODE-TOKEN ()) (struct SED OPCODE-TOKEN ()) (struct SEI OPCODE-TOKEN ()) (struct STA OPCODE-TOKEN ())
(struct STX OPCODE-TOKEN ()) (struct STY OPCODE-TOKEN ()) (struct TAX OPCODE-TOKEN ()) (struct TAY OPCODE-TOKEN ())
(struct TXA OPCODE-TOKEN ()) (struct TSX OPCODE-TOKEN ()) (struct TXS OPCODE-TOKEN ()) (struct TYA OPCODE-TOKEN ())

(define (op-string->OPCODE-TOKEN op-str)
  (case op-str
    [("ADC") (ADC)]
    [("AND") (AND)]
    [("ASL") (ASL)]
    [("BCC") (BCC)]
    [("BCS") (BCS)]
    [("BEQ") (BEQ)]
    [("BIT") (BIT)]
    [("BMI") (BMI)]
    [("BNE") (BNE)]
    [("BPL") (BPL)]
    [("BRK") (BRK)]
    [("BVC") (BVC)]
    [("BVS") (BVS)]
    [("CLC") (CLC)]
    [("CLD") (CLD)]
    [("CLI") (CLI)]
    [("CLV") (CLV)]
    [("CMP") (CMP)]
    [("CPX") (CPX)]
    [("CPY") (CPY)]
    [("DEC") (DEC)]
    [("DEX") (DEX)]
    [("DEY") (DEY)]
    [("EOR") (EOR)]
    [("INC") (INC)]
    [("INX") (INX)]
    [("INY") (INY)]
    [("JMP") (JMP)]
    [("JSR") (JSR)]
    [("LDA") (LDA)]
    [("LDX") (LDX)]
    [("LDY") (LDY)]
    [("LSR") (LSR)]
    [("NOP") (NOP)]
    [("ORA") (ORA)]
    [("PHA") (PHA)]
    [("PHP") (PHA)]
    [("PLA") (PLA)]
    [("PLP") (PLP)]
    [("ROL") (ROL)]
    [("ROR") (ROR)]
    [("RTI") (RTI)]
    [("RTS") (RTS)]
    [("SBC") (SBC)]
    [("SEC") (SEC)]
    [("SED") (SED)]
    [("SEI") (SEI)]
    [("STA") (STA)]
    [("STX") (STX)]
    [("STY") (STY)]
    [("TAX") (TAX)]
    [("TAY") (TAY)]
    [("TXA") (TXA)]
    [("TSX") (TSX)]
    [("TXS") (TXS)]
    [("TYA") (TYA)]))

(define opcode?
  (member  _ '("ADC"  "AND"  "ASL"  "BCC" 
               "BCS"  "BEQ"  "BIT"  "BMI" 
               "BNE"  "BPL"  "BRK"  "BVC" 
               "BVS"  "CLC"  "CLD"  "CLI" 
               "CLV"  "CMP"  "CPX"  "CPY" 
               "DEC"  "DEX"  "DEY"  "EOR" 
               "INC"  "INX"  "INY"  "JMP" 
               "JSR"  "LDA"  "LDX"  "LDY" 
               "LSR"  "NOP"  "ORA"  "PHA" 
               "PHP"  "PLA"  "PLP"  "ROL" 
               "ROR"  "RTI"  "RTS"  "SBC" 
               "SEC"  "SED"  "SEI"  "STA" 
               "STX"  "STY"  "TAX"  "TAY" 
               "TXA"  "TSX"  "TXS"  "TYA")))

(define (read-opcode)
  (expect-str 'opcode opcode? 3))

(define (parse-opcode)
  (read-opcode)
  (op-string->OPCODE-TOKEN (read-string 3)))

#| op-exprs |#


(with-input-from-string " ADC $1A ;howdy\n wii "
  (λ ()
     (skip-whitespace)
     (parse-opcode)
     (read-whitespace+)
     (parse-number)
     (read-whitespace+)
     (skip-comment)))
