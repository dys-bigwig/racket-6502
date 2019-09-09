#lang racket/base
(require (only-in parser-tools/lex define-lex-abbrev)
         "mnemonic.rkt"
         "number.rkt"
         "directive.rkt")
(provide reserved/l)

(define-lex-abbrev reserved/l
                   (:or opcode/l directive/l))
