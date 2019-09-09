
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
        [() 'eof])
      (Line
        [(mnemonic Operand?) (Opcode $1 $2)])
      (Int
        [(8-bit-int) $1]
        [(16-bit-int) $1])
      (Operand?
        [(hashtag Int) (Operand $2 'IMM #f)]
        [(8-bit-int X/Y?) (Operand $1 'ZP $2)]
        [(16-bit-int X/Y?) (Operand $1 'ABS $2)]
        [(lparen Int Indirect) (Operand $2 'IND $3)]
        [(A) 'A]
        [() (Operand #f 'IMP #f)])
      (X/Y?
        [(X) 'X]
        [(Y) 'Y]
        [() #f])
      (Rparen?
        [(rparen) (void)]
        [() (void)])
      (Indirect
        [(Rparen? Indexed?) $2])
      (Indexed
        [(comma X/Y? Rparen?) $2])
      (Indexed?
        [(Indexed) $1]
        [() #f])]))

;JMP ($4008) | LDA ($22,x) | LDA ($22),y

(define (lex+parse str)
  (define in (open-input-string str))
  (for/list ([line (in-producer (thunk (test-parse (thunk (test-lex in)))))]
             #:break (equal? line 'eof))
    (car line)))

(lex+parse "INC ($1100)")
