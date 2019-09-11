#lang racket
(require "../lexer/lex.rkt"
         parser-tools/yacc
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
    [start Line*]
    [end eof]
    [error (λ (tok-ok? name val)
              (error (format "~a ~a" name val)))]
    [debug "debug.txt"]
    ;[yacc-output "out.y"]
    [grammar
      (Line*
        [(Line Line*) (cons $1 $2)]
        [() '()])
      (Line
        [(mnemonic Operand? newline) (Instruction (string->symbol $1) $2)]
        [(identifier equals int newline) (Equals $1 $3)]
        [(label Newline?) (Label $1)]
        [(db Int/Identifier* newline) (Db $2)])
      (Int/Identifier*
        [(Int/Identifier Int/Identifier*) (cons $1 $2)]
        [() '()])
      (Newline?
        [(newline) (void)]
        [() (void)])
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

(define (lex+parse path)
  (call-with-input-file path
    (λ (in) 
       (parse (λ () (lex in))))))
