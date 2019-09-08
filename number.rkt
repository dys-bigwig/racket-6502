#lang racket
(require parser-tools/lex (prefix-in : parser-tools/lex-sre))
(provide (all-defined-out))

(define-lex-abbrev hex-digit (:or numeric (char-range #\a #\f) (char-range #\A #\F)))

(define-lex-abbrev number/l (:or (:+ numeric) 
                                 (:: #\$ (:+ hex-digit))))
