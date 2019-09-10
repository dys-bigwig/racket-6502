#lang racket/base
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
         get-operand-mode
         Instruction?
         Label?
         Identifier?)

(define (get-operand-mode operand)
  (cons (if (symbol=? (Operand-mode operand)
                      'ZP/ABS)
          (if (> (Operand-value operand)
                 #xFF)
            'ABS
            'ZP)
          (Operand-mode operand))
        (Operand-index operand)))

(struct Instruction (name operand) #:transparent)
(struct Operand (value mode index) #:transparent)
(struct Label (name) #:transparent)
(struct Identifier (name) #:transparent)

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
        [(identifier colon) (Label $1)]
        [(identifier) (Identifier $1)])
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
