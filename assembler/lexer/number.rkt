#lang racket/base
(require (only-in parser-tools/lex define-lex-abbrev numeric)
         (prefix-in : parser-tools/lex-sre))
(provide (all-defined-out))

(define-lex-abbrev hex-digit (:or numeric (char-range #\a #\f) (char-range #\A #\F)))

(define-lex-abbrev number/l (:or (:+ numeric) 
                                 (:: #\$ (:+ hex-digit))))
