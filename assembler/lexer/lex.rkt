#lang racket/base
(require parser-tools/lex (prefix-in : parser-tools/lex-sre))
(require "transformer.rkt" "mnemonic.rkt" "number.rkt" "reserved.rkt" "directive.rkt")
(provide lex delimiters directives mnemonics atoms letters)

(define-empty-tokens delimiters (lparen rparen lbracket dollar rbracket hashtag comma newline colon eof))
(define-empty-tokens directives (equals db))
(define-tokens mnemonics (mnemonic))
(define-tokens atoms (int identifier label))
(define-empty-tokens letters (A X Y))

(define-lex-abbrev identifier/l (:& (complement reserved/l)
                                    (complement number/l)
                                    (:+ (:or alphabetic numeric))))

(define-lex-abbrev label/l (:: identifier/l #\:))

(define lex
  (lexer
    [(:+ (:or #\space #\tab )) (lex input-port)]
    [(:+ #\newline) (token-newline)]
    [#\# (token-hashtag)]
    [#\: (token-colon)]
    [db/l (token-db)]
    [mnemonic/l (token-mnemonic lexeme)]
    [identifier/l (token-identifier lexeme)]
    [#\= (token-equals)]
    [label/l (token-label lexeme)]
    [(:+ numeric) (token-int (string->number lexeme))]
    [(:: #\$ (:+ hex-digit)) (token-int (string->number (substring lexeme 1) 16))]
    [#\( (token-lparen)]
    [#\) (token-rparen)]
    [#\, (token-comma)]
    [(:char-ci #\A) (token-A)]
    [(:char-ci #\X) (token-X)]
    [(:char-ci #\Y) (token-Y)]))
