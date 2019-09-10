#lang racket/base
(require "../lexer/lex.rkt"
         parser-tools/yacc
         racket/require)
(provide lex+parse
         (rename-out (Instruction-name get-instruction-name)
                     (Instruction-operand get-operand)
                     (Operand-val get-operand-val)
                     (Operand-index get-operand-index)
                     (Label-name get-label-name))
         get-operand-mode
         Instruction?
         Label?)

(define (get-operand-mode operand)
  (cons (Operand-mode operand)
        (Operand-index operand)))

(struct Instruction (name operand) #:transparent)
(struct Operand (val mode index) #:transparent)
(struct Label (name) #:transparent)

(define parse
  (parser
    [tokens mnemonics atoms delimiters letters]
    [start Line*]
    [end eof]
    [error (λ (tok-ok? name val)
              (error (format "~a ~a" name val)))]
    ;[debug "debug.txt"]
    ;[yacc-output "out.y"]
    [grammar
      (Line*
        [(Line Line*) (cons $1 $2)]
        [() '()])
      (Line
        [(mnemonic Operand?) (Instruction (string->symbol $1) $2)]
        [(identifier colon) (Label $1)])
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
