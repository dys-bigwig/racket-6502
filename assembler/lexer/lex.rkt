#lang racket/base
(require parser-tools/lex (prefix-in : parser-tools/lex-sre))
(require "transformer.rkt" "mnemonic.rkt" "number.rkt" "reserved.rkt")
(provide lex delimiters directives mnemonics atoms letters)

(define-empty-tokens delimiters (lparen rparen lbracket dollar rbracket hashtag comma newline colon eof))
(define-empty-tokens directives (equate db))
(define-tokens mnemonics (mnemonic))
(define-tokens atoms (int identifier))
(define-empty-tokens letters (A X Y))

(define-lex-abbrev identifier/l (:& (complement reserved/l)
                                    (:+ (:or alphabetic numeric))))

(define lex
  (lexer
    [(:+ (:or #\space #\tab #\newline)) (lex input-port)]
    [#\# (token-hashtag)]
    [#\: (token-colon)]
    [mnemonic/l (token-mnemonic lexeme)]
    [identifier/l (token-identifier lexeme)]
    [(:+ numeric) (token-int (string->number lexeme))]
    [(:: #\$ (:+ hex-digit)) (token-int (string->number (substring lexeme 1) 16))]
    [#\( (token-lparen)]
    [#\) (token-rparen)]
    [#\, (token-comma)]
    [(:char-ci #\A) (token-A)]
    [(:char-ci #\X) (token-X)]
    [(:char-ci #\Y) (token-Y)]))
