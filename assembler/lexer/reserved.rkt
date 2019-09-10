#lang racket/base
(require (only-in parser-tools/lex define-lex-abbrev)
         (prefix-in : (only-in parser-tools/lex-sre or))         "mnemonic.rkt"
         "number.rkt"
         "directive.rkt"
         "transformer.rkt")
(provide reserved/l)

(define-lex-abbrev reserved/l
                   (:or mnemonic/l
                        directive/l
                        (:char-ci #\A)
                        (:char-ci #\X)
                        (:char-ci #\Y)))
