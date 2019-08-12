#lang racket
(require megaparsack megaparsack/text)
(require data/monad)
(require data/functor)
(require data/applicative)
(require fancy-app)

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

(define mode-acc/p
  (char/p #\a))

(define num-lit/p
  (do
    (or/p (try/p hex-lit/p)
          dec-lit/p)))

(define x/p
  (do (char/p #\,)
      (char-ci/p #\x)))

(define y/p
  (do (char/p #\,)
      (char-ci/p #\y)))

(define mode-imm/p
  (do
    (char/p #\#)
    (rand <- (guard/p num-lit/p
                      (<= _ #xffff)))
    (pure rand)))

(define mode-zp/p
  (guard/p (or/p hex-lit/p
                 dec-lit/p)
           (conjoin (<= _ #xFF))))

(define mode-zp-x/p
  (do
    [rand <- mode-zp/p]
    x/p
    (pure rand)))

(define mode-zp-y/p
  (do
    [rand <- mode-zp/p]
    y/p
    (pure rand)))

(define label/p
  (do
    [label <- (many+/p letter/p)]
    (pure (list->string label))))

(define mode-rel/p
  label/p)

(define mode-abs/p
  (guard/p (or/p hex-lit/p
                 num-lit/p)
                (conjoin (> _ #xFF)
                         (<= _ #xFFFF))))

(define mode-abs-x/p
  (do
    [rand <- mode-abs/p]
    x/p
    (pure rand)))

(define mode-abs-y/p
  (do
    [rand <- mode-abs/p]
    y/p
    (pure rand)))

(define mode-ind/p
  mode-abs/p)

(define mode-ind-x/p
  (do
    (char/p #\()
    (rand <- mode-zp-x/p)
    (char/p #\))
    (pure rand)))

(define mode-ind-y/p
  (do
    (char/p #\()
    (rand <- mode-zp-y/p)
    (char/p #\))
    (pure rand)))

(define opcode/p (or/p (string-ci/p "ADC") (string-ci/p "AND") (string-ci/p "ASL") (string-ci/p "BCC") (string-ci/p "BCS")
                       (string-ci/p "BEQ") (string-ci/p "BIT") (string-ci/p "BMI") (string-ci/p "BNE") (string-ci/p "BPL")
                       (string-ci/p "BRK") (string-ci/p "BVC") (string-ci/p "BVS") (string-ci/p "CLC") (string-ci/p "CLD")
                       (string-ci/p "CLI") (string-ci/p "CLV") (string-ci/p "CMP") (string-ci/p "CPX") (string-ci/p "CPY")
                       (string-ci/p "DEC") (string-ci/p "DEX") (string-ci/p "DEY") (string-ci/p "EOR") (string-ci/p "INC")
                       (string-ci/p "INX") (string-ci/p "INY") (string-ci/p "JMP") (string-ci/p "JSR") (string-ci/p "LDA")
                       (string-ci/p "LDX") (string-ci/p "LDY") (string-ci/p "LSR") (string-ci/p "NOP") (string-ci/p "ORA")
                       (string-ci/p "PHA") (string-ci/p "PHP") (string-ci/p "PLA") (string-ci/p "PLP") (string-ci/p "ROL")
                       (string-ci/p "ROR") (string-ci/p "RTI") (string-ci/p "RTS") (string-ci/p "SBC") (string-ci/p "SEC")
                       (string-ci/p "SED") (string-ci/p "SEI") (string-ci/p "STA") (string-ci/p "STX") (string-ci/p "STY")
                       (string-ci/p "TAX") (string-ci/p "TAY") (string-ci/p "TXA") (string-ci/p "TSX") (string-ci/p "TXS")
                       (string-ci/p "TYA")))

(parse-string mode-ind-y/p "($20,Y)")
