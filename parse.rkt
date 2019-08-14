#lang racket
(require megaparsack megaparsack/text)
(require data/monad)
(require data/functor)
(require data/applicative)
(require fancy-app)
(require data/either)

(define hex-digit/p
  (or/p (char-between/p #\a
                        #\f)
        (char-between/p #\A
                        #\F)
        digit/p))

(define hex-digit+/p (many+/p hex-digit/p))

(define hex-lit/p
  (do
    (char/p #\$)
    [num <- hex-digit+/p]
    (pure (string->number (list->string num) 16))))

(define dec-lit/p
  (do
    [num <- (many+/p digit/p)]
    (pure (string->number (list->string num)))))

(define num-lit/p
  (do
    (or/p (try/p hex-lit/p)
          dec-lit/p)))

(struct LABEL-REF (name) #:transparent)

(define label-ref/p
  (do
    [label <- (many+/p letter/p)]
    (pure (LABEL-REF (list->string label)))))

(define x/p
  (do (char/p #\,)
    (char-ci/p #\x)))

(define y/p
  (do (char/p #\,)
    (char-ci/p #\y)))

(struct ACC ())

(define mode-acc/p
  (do
    (char-ci/p #\a)
    (pure (ACC))))

(struct IMM (val) #:transparent)

(define mode-imm/p
  (do
    (char/p #\#)
    (rand <- (guard/p num-lit/p
                      (<= _ #xFFFF)))
    (pure (IMM rand))))

(struct ZP (val) #:transparent)

(define mode-zp/p
  (do
    [rand <- (guard/p num-lit/p
                      (<= _ #xFF))]
    (pure (ZP rand))))

(struct ZP-X (val) #:transparent)

(define mode-zp-x/p
  (do
    [rand <- mode-zp/p]
    x/p
    (pure (ZP-X (ZP-val rand)))))

(struct ZP-Y (val) #:transparent)

(define mode-zp-y/p
  (do
    [rand <- mode-zp/p]
    y/p
    (pure (ZP-Y (ZP-val rand)))))

(struct REL (offset) #:transparent)

(define mode-rel/p
  (do
    [rand <- (guard/p num-lit/p
                      (<= _ #xFF))]
    (pure (REL rand))))

(struct ABS (val) #:transparent)

(define mode-abs/p
  (do
    [rand <- (guard/p num-lit/p
                      (conjoin (> _ #xFF)
                               (<= _ #xFFFF)))]
    (pure (ABS rand))))

(struct ABS-X (val) #:transparent)

(define mode-abs-x/p
  (do
    [rand <- mode-abs/p]
    x/p
    (pure (ABS-X (ABS-val rand)))))

(struct ABS-Y (val) #:transparent)

(define mode-abs-y/p
  (do
    [rand <- mode-abs/p]
    y/p
    (pure (ABS-Y (ABS-val rand)))))

(struct IND (addr) #:transparent)

(define mode-ind/p
  (do 
    (char/p #\()
    [rand <- (guard/p num-lit/p
                      (conjoin (> _ #xFF)
                               (<= _ #xFFFF)))]
    (char/p #\))
    (pure (IND rand))))

(struct IND-X (val) #:transparent)

(define mode-ind-x/p
  (do
    (char/p #\()
    (rand <- mode-zp-x/p)
    (char/p #\))
    (pure (IND-X (ZP-X-val rand)))))

(struct IND-Y (val) #:transparent)

(define mode-ind-y/p
  (do
    (char/p #\()
    (rand <- mode-zp-y/p)
    (char/p #\))
    (pure (IND-Y (ZP-Y-val rand)))))

(struct IMPL () #:transparent)

(define mode-impl/p
  (do
    void/p
    (pure (IMPL))))

(struct EQU (name val) #:transparent)

(define equ/p
  (do
    [label <- (many+/p (char-not-in/p " "))]
    (many+/p (char/p #\space))
    (string-ci/p "EQU")
    (many+/p (char/p #\space))
    [val <- (many+/p (char-not-in/p "\n"))]
    (pure (EQU (list->string label) (list->string val)))))

(struct BYTE (val) #:transparent)

(define byte/p
  (do
    (string-ci/p "BYTE")
    (many+/p (char/p #\space))
    [val <- num-lit/p]
    (pure (BYTE val))))

(struct OP-EXPR (opcode operand) #:transparent)
(struct ADC ()) (struct AND ()) (struct ASL ()) (struct BCC ())
(struct BCS ()) (struct BEQ ()) (struct BIT ()) (struct BMI ())
(struct BNE ()) (struct BPL ()) (struct BRK ()) (struct BVC ())
(struct BVS ()) (struct CLC ()) (struct CLD ()) (struct CLI ())
(struct CLV ()) (struct CMP ()) (struct CPX ()) (struct CPY ())
(struct DEC ()) (struct DEX ()) (struct DEY ()) (struct EOR ())
(struct INC ()) (struct INX ()) (struct INY ()) (struct JMP ())
(struct JSR ()) (struct LDA ()) (struct LDX ()) (struct LDY ())
(struct LSR ()) (struct NOP ()) (struct ORA ()) (struct PHA ())
(struct PHP ()) (struct PLA ()) (struct PLP ()) (struct ROL ())
(struct ROR ()) (struct RTI ()) (struct RTS ()) (struct SBC ())
(struct SEC ()) (struct SED ()) (struct SEI ()) (struct STA ())
(struct STX ()) (struct STY ()) (struct TAX ()) (struct TAY ())
(struct TXA ()) (struct TSX ()) (struct TXS ()) (struct TYA ())

(define (op-string->OP op-str)
  (define OP-hash
    (hash "ADC" (ADC) "AND" (AND) "ASL" (ASL) "BCC" (BCC)
          "BCS" (BCS) "BEQ" (BEQ) "BIT" (BIT) "BMI" (BMI)
          "BNE" (BNE) "BPL" (BPL) "BRK" (BRK) "BVC" (BVC)
          "BVS" (BVS) "CLC" (CLC) "CLD" (CLD) "CLI" (CLI)
          "CLV" (CLV) "CMP" (CMP) "CPX" (CPX) "CPY" (CPY)
          "DEC" (DEC) "DEX" (DEX) "DEY" (DEY) "EOR" (EOR)
          "INC" (INC) "INX" (INX) "INY" (INY) "JMP" (JMP)
          "JSR" (JSR) "LDA" (LDA) "LDX" (LDX) "LDY" (LDY)
          "LSR" (LSR) "NOP" (NOP) "ORA" (ORA) "PHA" (PHA)
          "PHP" (PHA) "PLA" (PLA) "PLP" (PLP) "ROL" (ROL)
          "ROR" (ROR) "RTI" (RTI) "RTS" (RTS) "SBC" (SBC)
          "SEC" (SEC) "SED" (SED) "SEI" (SEI) "STA" (STA)
          "STX" (STX) "STY" (STY) "TAX" (TAX) "TAY" (TAY)
          "TXA" (TXA) "TSX" (TSX) "TXS" (TXS) "TYA" (TYA)))
  (hash-ref OP-hash op-str)) 

(define opcode/p
  (do
    [op <- (or/p (try/p (string-ci/p "ADC")) (try/p (string-ci/p "AND")) (try/p (string-ci/p "ASL")) (try/p (string-ci/p "BCC"))
                 (try/p (string-ci/p "BCS")) (try/p (string-ci/p "BEQ")) (try/p (string-ci/p "BIT")) (try/p (string-ci/p "BMI"))
                 (try/p (string-ci/p "BNE")) (try/p (string-ci/p "BPL")) (try/p (string-ci/p "BRK")) (try/p (string-ci/p "BVC"))
                 (try/p (string-ci/p "BVS")) (try/p (string-ci/p "CLC")) (try/p (string-ci/p "CLD")) (try/p (string-ci/p "CLI"))
                 (try/p (string-ci/p "CLV")) (try/p (string-ci/p "CMP")) (try/p (string-ci/p "CPX")) (try/p (string-ci/p "CPY"))
                 (try/p (string-ci/p "DEC")) (try/p (string-ci/p "DEX")) (try/p (string-ci/p "DEY")) (try/p (string-ci/p "EOR"))
                 (try/p (string-ci/p "INC")) (try/p (string-ci/p "INX")) (try/p (string-ci/p "INY")) (try/p (string-ci/p "JMP")) 
                 (try/p (string-ci/p "JSR")) (try/p (string-ci/p "LDA")) (try/p (string-ci/p "LDX")) (try/p (string-ci/p "LDY"))
                 (try/p (string-ci/p "LSR")) (try/p (string-ci/p "NOP")) (try/p (string-ci/p "ORA")) (try/p (string-ci/p "PHA"))
                 (try/p (string-ci/p "PHP")) (try/p (string-ci/p "PLA")) (try/p (string-ci/p "PLP")) (try/p (string-ci/p "ROL"))
                 (try/p (string-ci/p "ROR")) (try/p (string-ci/p "RTI")) (try/p (string-ci/p "RTS")) (try/p (string-ci/p "SBC"))
                 (try/p (string-ci/p "SEC")) (try/p (string-ci/p "SED")) (try/p (string-ci/p "SEI")) (try/p (string-ci/p "STA"))
                 (try/p (string-ci/p "STX")) (try/p (string-ci/p "STY")) (try/p (string-ci/p "TAX")) (try/p (string-ci/p "TAY"))
                 (try/p (string-ci/p "TXA")) (try/p (string-ci/p "TSX")) (try/p (string-ci/p "TXS")) (try/p (string-ci/p "TYA")))]
    (pure (op-string->OP op))))

(define operand/p
  (or/p (try/p mode-acc/p)
        (try/p mode-imm/p)
        (try/p mode-zp/p)
        (try/p mode-zp-x/p)
        (try/p mode-zp-y/p)
        (try/p mode-rel/p)
        (try/p mode-abs/p)
        (try/p mode-abs-x/p)
        (try/p mode-abs-y/p)
        (try/p mode-ind/p)
        (try/p mode-ind-x/p)
        (try/p mode-ind-y/p)
        (try/p mode-impl/p)))

(define opcode&operand/p
  (do
    [op <- opcode/p]
    (char/p #\space)
    [rand <- operand/p]
    (pure (OP-EXPR op rand))))

(define expr/p
  (or/p (try/p equ/p)
        (try/p opcode&operand/p)) )

(define expr*/p
  (many/p expr/p #:sep (many/p (char/p #\newline))))

(parse-string expr*/p "PI EQU 3.14\nLDA #$41")
