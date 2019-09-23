#lang racket
(require racket/generator)
(require "../lexer/lex.rkt"
         parser-tools/yacc
         parser-tools/lex
         racket/require
         (only-in racket/bool symbol=?))
(provide lex+parse
         (rename-out (Instruction-name get-instruction-name)
                     (Instruction-operand get-operand)
                     (Operand-value get-operand-value)
                     (Operand-index get-operand-index)
                     (Label-name get-label-name)
                     (Identifier-name get-identifier-name))
         Instruction?
         Label?
         Identifier?)

(struct Instruction (name operand) #:transparent)
(struct Operand (value mode index) #:transparent)
(struct Label (name) #:transparent)
(struct Identifier (name) #:transparent)
(struct Equals (name value) #:transparent)
(struct Db (bytes) #:transparent)

(define parse
  (parser
    [tokens mnemonics atoms delimiters letters directives]
    [start Line]
    [end newline]
    [error (λ (tok-ok? name val)
              (error (format "~a ~a" name val)))]
    [debug "debug.txt"]
    ;[yacc-output "out.y"]
    [grammar
      (Line
        [(mnemonic Operand?) (Instruction (string->symbol $1) $2)]
        [(identifier equals Int/Identifier) (Equals $1 $3)]
        [(label) (Label $1)]
        [(db Int/Identifier*) (Db $2)]
        [() (void)])
      (Int/Identifier*
        [(Int/Identifier Int/Identifier*) (cons $1 $2)]
        [() '()])
      (Int/Identifier
        [(int) $1]
        [(identifier) $1])
      (Operand?
        [() (Operand #f 'IMP #f)]
        [(A) (Operand #f 'ACC #f)]
        [(Int/Identifier X/Y?) (Operand $1 'ZP/ABS $2)]
        [(hashtag Int/Identifier) (Operand $2 'IMM #f)]
        [(lparen Int/Identifier Indirect-indexed?) (Operand $2 'IND $3)])
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

(define (lex+parse in)
  (parse (thunk (lex in))))
