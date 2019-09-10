        [(mnemonic Operand?) (Instruction $1 $2)])
      (Operand?
        [() (Operand #f 'IMP #f)]
        [(A) (Operand #f 'A #f)]
        [(int X/Y?) (Operand $1 'ZP/ABS $2)]
        [(hashtag int) (Operand $2 'IMM #f)]
        [(lparen int Indirect-indexed?) (Operand $2 'IND $3)])
      (X/Y?
        [(comma X) 'X]
        [(comma Y) 'Y]
        [() #f])
      (Y?
        [(comma Y) 'Y]
        [() #f])
      (Indirect-indexed?
        [(rparen Y?) $2]
        [(comma X rparen) 'X])]))

(define (parse-string str)
  (define in (open-input-string str))
  (parse (λ () (lex in))))

(define (lex+parse path)
  (call-with-input-file path
    (λ (in) 
       (parse (λ () (lex in))))))
