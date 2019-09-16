#lang racket/base
(require (only-in parser-tools/lex define-lex-abbrev)
         (prefix-in : parser-tools/lex-sre))
(require "transformer.rkt")
(provide (all-defined-out))

(define-lex-abbrev equate/l (:: (:char-ci #\E) (:char-ci #\Q) (:char-ci #\U)))

(define-lex-abbrev db/l (:: #\. (:char-ci #\D) (:char-ci #\B)))

(define-lex-abbrev directive/l (:or equate/l db/l))
