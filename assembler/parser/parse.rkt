#lang racket/base
(require "../lex/lex.rkt")
(require parser-tools/yacc
         (only-in racket/function thunk))

(struct Opcode (name operand) #:transparent)
(struct Operand (val mode index) #:transparent)

(define parse
  (parser
    [tokens mnemonics atoms delimiters letters]
    [start Line*]
    [end newline]
    [error (λ (tok-ok? name val)
              (error (format "~a ~a" name val)))]
    ;[debug "debug.txt"]
    ;[yacc-output "out.y"]
    [grammar
      (Line*
        [(Line Line*) (cons $1 $2)]
        [() 'eof])
      (Line
        [(mnemonic Operand?) (Opcode $1 $2)])
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

(define (lex+parse str)
  (define in (open-input-string str))
  (for/list ([line (in-producer (thunk (parse (thunk (lex in)))))]
             #:break (equal? line 'eof))
    (car line)))

(lex+parse "INC ($00),y\nLDA #$22")
