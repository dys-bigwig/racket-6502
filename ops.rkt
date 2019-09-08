#lang racket
(require json)
(require fancy-app)
(require "mode.rkt")
(provide (all-defined-out))

(define opcode-vector (make-vector 255))

(define hex-string->number
  (compose (string->number _ 16) (substring _ 1)))

(define instructions
  '#hash(("ADC"
          .
          #(#hash((hex . 105) (length . 2) (mode . "Immediate"))
            #hash((hex . 101) (length . 2) (mode . "ZeroPage"))
            #hash((hex . 117) (length . 2) (mode . "ZeroPage,X"))
            #hash((hex . 109) (length . 3) (mode . "Absolute"))
            #hash((hex . 125) (length . 3) (mode . "Absolute,X"))
            #hash((hex . 121) (length . 3) (mode . "Absolute,Y"))
            #hash((hex . 97) (length . 2) (mode . "(Indirect,X)"))
            #hash((hex . 113) (length . 2) (mode . "(Indirect),Y"))))
         ("AND"
          .
          #(#hash((hex . 41) (length . 2) (mode . "Immediate"))
            #hash((hex . 37) (length . 2) (mode . "ZeroPage"))
            #hash((hex . 53) (length . 2) (mode . "ZeroPage,X"))
            #hash((hex . 45) (length . 3) (mode . "Absolute"))
            #hash((hex . 61) (length . 3) (mode . "Absolute,X"))
            #hash((hex . 57) (length . 3) (mode . "Absolute,Y"))
            #hash((hex . 33) (length . 2) (mode . "(Indirect,X)"))
            #hash((hex . 49) (length . 2) (mode . "(Indirect),Y"))))
         ("ASL"
          .
          #(#hash((hex . 10) (length . 1) (mode . "Accumulator"))
            #hash((hex . 6) (length . 2) (mode . "ZeroPage"))
            #hash((hex . 22) (length . 2) (mode . "ZeroPage,X"))
            #hash((hex . 14) (length . 3) (mode . "Absolute"))
            #hash((hex . 30) (length . 3) (mode . "Absolute,X"))))
         ("BCC" . #(#hash((hex . 144) (length . 2) (mode . "Relative"))))
         ("BCS" . #(#hash((hex . 176) (length . 2) (mode . "Relative"))))
         ("BEQ" . #(#hash((hex . 240) (length . 2) (mode . "Relative"))))
         ("BIT"
          .
          #(#hash((hex . 36) (length . 2) (mode . "ZeroPage"))
            #hash((hex . 44) (length . 3) (mode . "Absolute"))))
         ("BMI" . #(#hash((hex . 48) (length . 2) (mode . "Relative"))))
         ("BNE" . #(#hash((hex . 208) (length . 2) (mode . "Relative"))))
         ("BPL" . #(#hash((hex . 16) (length . 2) (mode . "Relative"))))
         ("BRK" . #(#hash((hex . 0) (length . 1) (mode . "Implied"))))
         ("BVC" . #(#hash((hex . 80) (length . 2) (mode . "Relative"))))
         ("BVS" . #(#hash((hex . 112) (length . 2) (mode . "Relative"))))
         ("CLC" . #(#hash((hex . 24) (length . 1) (mode . "Implied"))))
         ("CLD" . #(#hash((hex . 216) (length . 1) (mode . "Implied"))))
         ("CLI" . #(#hash((hex . 88) (length . 1) (mode . "Implied"))))
         ("CLV" . #(#hash((hex . 184) (length . 1) (mode . "Implied"))))
         ("CMP"
          .
          #(#hash((hex . 201) (length . 2) (mode . "Immediate"))
            #hash((hex . 197) (length . 2) (mode . "ZeroPage"))
            #hash((hex . 213) (length . 2) (mode . "ZeroPage,X"))
            #hash((hex . 205) (length . 3) (mode . "Absolute"))
            #hash((hex . 221) (length . 3) (mode . "Absolute,X"))
            #hash((hex . 217) (length . 3) (mode . "Absolute,Y"))
            #hash((hex . 193) (length . 2) (mode . "(Indirect,X)"))
            #hash((hex . 209) (length . 2) (mode . "(Indirect),Y"))))
         ("CPX"
          .
          #(#hash((hex . 224) (length . 2) (mode . "Immediate"))
            #hash((hex . 228) (length . 2) (mode . "ZeroPage"))
            #hash((hex . 236) (length . 3) (mode . "Absolute"))))
         ("CPY"
          .
          #(#hash((hex . 192) (length . 2) (mode . "Immediate"))
            #hash((hex . 196) (length . 2) (mode . "ZeroPage"))
            #hash((hex . 204) (length . 3) (mode . "Absolute"))))
         ("DEC"
          .
          #(#hash((hex . 198) (length . 2) (mode . "ZeroPage"))
            #hash((hex . 214) (length . 2) (mode . "ZeroPage,X"))
            #hash((hex . 206) (length . 3) (mode . "Absolute"))
            #hash((hex . 222) (length . 3) (mode . "Absolute,X"))))
         ("DEX" . #(#hash((hex . 202) (length . 1) (mode . "Implied"))))
         ("DEY" . #(#hash((hex . 136) (length . 1) (mode . "Implied"))))
         ("EOR"
          .
          #(#hash((hex . 73) (length . 2) (mode . "Immediate"))
            #hash((hex . 69) (length . 2) (mode . "ZeroPage"))
            #hash((hex . 85) (length . 2) (mode . "ZeroPage,X"))
            #hash((hex . 77) (length . 3) (mode . "Absolute"))
            #hash((hex . 93) (length . 3) (mode . "Absolute,X"))
            #hash((hex . 89) (length . 3) (mode . "Absolute,Y"))
            #hash((hex . 65) (length . 2) (mode . "(Indirect,X)"))
            #hash((hex . 81) (length . 2) (mode . "(Indirect),Y"))))
         ("INC"
          .
          #(#hash((hex . 230) (length . 2) (mode . "ZeroPage"))
            #hash((hex . 246) (length . 2) (mode . "ZeroPage,X"))
            #hash((hex . 238) (length . 3) (mode . "Absolute"))
            #hash((hex . 254) (length . 3) (mode . "Absolute,X"))))
         ("INX" . #(#hash((hex . 232) (length . 1) (mode . "Implied"))))
         ("INY" . #(#hash((hex . 200) (length . 1) (mode . "Implied"))))
         ("JMP"
          .
          #(#hash((hex . 76) (length . 3) (mode . "Absolute"))
            #hash((hex . 108) (length . 3) (mode . "Indirect "))))
         ("JSR" . #(#hash((hex . 32) (length . 3) (mode . "Absolute"))))
         ("LDA"
          .
          #(#hash((hex . 169) (length . 2) (mode . "Immediate"))
            #hash((hex . 165) (length . 2) (mode . "ZeroPage"))
            #hash((hex . 181) (length . 2) (mode . "ZeroPage,X"))
            #hash((hex . 173) (length . 3) (mode . "Absolute"))
            #hash((hex . 189) (length . 3) (mode . "Absolute,X"))
            #hash((hex . 185) (length . 3) (mode . "Absolute,Y"))
            #hash((hex . 161) (length . 2) (mode . "(Indirect,X)"))
            #hash((hex . 177) (length . 2) (mode . "(Indirect),Y"))))
         ("LDX"
          .
          #(#hash((hex . 162) (length . 2) (mode . "Immediate"))
            #hash((hex . 166) (length . 2) (mode . "ZeroPage"))
            #hash((hex . 182) (length . 2) (mode . "ZeroPage,Y"))
            #hash((hex . 174) (length . 3) (mode . "Absolute"))
            #hash((hex . 190) (length . 3) (mode . "Absolute,Y"))))
         ("LDY"
          .
          #(#hash((hex . 160) (length . 2) (mode . "Immediate"))
            #hash((hex . 164) (length . 2) (mode . "ZeroPage"))
            #hash((hex . 180) (length . 2) (mode . "ZeroPage,X"))
            #hash((hex . 172) (length . 3) (mode . "Absolute"))
            #hash((hex . 188) (length . 3) (mode . "Absolute,X"))))
         ("LSR"
          .
          #(#hash((hex . 74) (length . 1) (mode . "Accumulator"))
            #hash((hex . 70) (length . 2) (mode . "ZeroPage"))
            #hash((hex . 86) (length . 2) (mode . "ZeroPage,X"))
            #hash((hex . 78) (length . 3) (mode . "Absolute"))
            #hash((hex . 94) (length . 3) (mode . "Absolute,X"))))
         ("NOP" . #(#hash((hex . 234) (length . 1) (mode . "Implied"))))
         ("ORA"
          .
          #(#hash((hex . 9) (length . 2) (mode . "Immediate"))
            #hash((hex . 5) (length . 2) (mode . "ZeroPage"))
            #hash((hex . 21) (length . 2) (mode . "ZeroPage,X"))
            #hash((hex . 13) (length . 3) (mode . "Absolute"))
            #hash((hex . 29) (length . 3) (mode . "Absolute,X"))
            #hash((hex . 25) (length . 3) (mode . "Absolute,Y"))
            #hash((hex . 1) (length . 2) (mode . "(Indirect,X)"))
            #hash((hex . 17) (length . 2) (mode . "(Indirect),Y"))))
         ("PHA" . #(#hash((hex . 72) (length . 1) (mode . "Implied"))))
         ("PHP" . #(#hash((hex . 8) (length . 1) (mode . "Implied"))))
         ("PLA" . #(#hash((hex . 104) (length . 1) (mode . "Implied"))))
         ("PLP" . #(#hash((hex . 40) (length . 1) (mode . "Implied"))))
         ("ROL"
          .
          #(#hash((hex . 42) (length . 1) (mode . "Accumulator"))
            #hash((hex . 38) (length . 2) (mode . "ZeroPage"))
            #hash((hex . 54) (length . 2) (mode . "ZeroPage,X"))
            #hash((hex . 46) (length . 3) (mode . "Absolute"))
            #hash((hex . 62) (length . 3) (mode . "Absolute,X"))))
         ("ROR"
          .
          #(#hash((hex . 106) (length . 1) (mode . "Accumulator"))
            #hash((hex . 102) (length . 2) (mode . "ZeroPage"))
            #hash((hex . 118) (length . 2) (mode . "ZeroPage,X"))
            #hash((hex . 110) (length . 3) (mode . "Absolute"))
            #hash((hex . 126) (length . 3) (mode . "Absolute,X"))))
         ("RTI" . #(#hash((hex . 64) (length . 1) (mode . "Implied"))))
         ("RTS" . #(#hash((hex . 96) (length . 1) (mode . "Implied"))))
         ("SBC"
          .
          #(#hash((hex . 233) (length . 2) (mode . "Immediate"))
            #hash((hex . 229) (length . 2) (mode . "ZeroPage"))
            #hash((hex . 245) (length . 2) (mode . "ZeroPage,X"))
            #hash((hex . 237) (length . 3) (mode . "Absolute"))
            #hash((hex . 253) (length . 3) (mode . "Absolute,X"))
            #hash((hex . 249) (length . 3) (mode . "Absolute,Y"))
            #hash((hex . 225) (length . 2) (mode . "(Indirect,X)"))
            #hash((hex . 241) (length . 2) (mode . "(Indirect),Y"))))
         ("SEC" . #(#hash((hex . 56) (length . 1) (mode . "Implied"))))
         ("SED" . #(#hash((hex . 248) (length . 1) (mode . "Implied"))))
         ("SEI" . #(#hash((hex . 120) (length . 1) (mode . "Implied"))))
         ("STA"
          .
          #(#hash((hex . 133) (length . 2) (mode . "ZeroPage"))
            #hash((hex . 149) (length . 2) (mode . "ZeroPage,X"))
            #hash((hex . 141) (length . 3) (mode . "Absolute"))
            #hash((hex . 157) (length . 3) (mode . "Absolute,X"))
            #hash((hex . 153) (length . 3) (mode . "Absolute,Y"))
            #hash((hex . 129) (length . 2) (mode . "(Indirect,X)"))
            #hash((hex . 145) (length . 2) (mode . "(Indirect),Y"))))
         ("STX"
          .
          #(#hash((hex . 134) (length . 2) (mode . "ZeroPage"))
            #hash((hex . 150) (length . 2) (mode . "ZeroPage,Y"))
            #hash((hex . 142) (length . 3) (mode . "Absolute"))))
         ("STY"
          .
          #(#hash((hex . 132) (length . 2) (mode . "ZeroPage"))
            #hash((hex . 148) (length . 2) (mode . "ZeroPage,X"))
            #hash((hex . 140) (length . 3) (mode . "Absolute"))))
         ("TAX" . #(#hash((hex . 170) (length . 1) (mode . "Implied"))))
         ("TAY" . #(#hash((hex . 168) (length . 1) (mode . "Implied"))))
         ("TSX" . #(#hash((hex . 186) (length . 1) (mode . "Implied"))))
         ("TXA" . #(#hash((hex . 138) (length . 1) (mode . "Implied"))))
         ("TXS" . #(#hash((hex . 154) (length . 1) (mode . "Implied"))))
         ("TYA" . #(#hash((hex . 152) (length . 1) (mode . "Implied"))))))

(define (define-op name body)
  (let ([instruction (hash-ref instructions name)])
    (for ([elem (in-vector instruction)])
      (vector-set! opcode-vector (hash-ref elem 'hex)
                                 (hash 'mode (mode-string->mode (hash-ref elem 'mode))
                                       'length (hash-ref elem 'length)
                                       'procedure body)))))
