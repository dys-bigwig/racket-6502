#lang racket
(require fancy-app)
(require parser-tools/lex (prefix-in : parser-tools/lex-sre))
(require "transformer.rkt" "mnemonic.rkt" "number.rkt" "reserved.rkt" "directive.rkt")
;(provide lex delimiters directives mnemonics atoms letters)
(provide token-value (all-defined-out))

(define-empty-tokens delimiters (lparen rparen lbracket dollar rbracket hashtag comma newline colon eof))
(define-empty-tokens directives (equals db))
(define-tokens mnemonics (mnemonic))
(define-tokens atoms (number identifier label))
(define-empty-tokens letters (a x y))

(define-lex-abbrev identifier/l (:& (complement reserved/l)
                                    (complement number/l)
                                    (:+ (:or alphabetic numeric))))

(define-lex-abbrev label/l (:: identifier/l #\:))

(define lex-src
  (lexer-src-pos
    [(:+ (:or #\space #\tab)) (lex input-port)]
    [(:+ #\newline) (token-newline)]
    [#\# (token-hashtag)]
    [#\: (token-colon)]
    [db/l (token-db)]
    [mnemonic/l (token-mnemonic lexeme)]
    [identifier/l (token-identifier lexeme)]
    [#\= (token-equals)]
    [label/l (token-label lexeme)]
    [(:+ numeric) (token-number (string->number lexeme))]
    [(:: #\$ (:+ hex-digit)) (token-number (string->number (substring lexeme 1) 16))]
    [#\( (token-lparen)]
    [#\) (token-rparen)]
    [#\, (token-comma)]
    [(:char-ci #\A) (token-a)]
    [(:char-ci #\X) (token-x)]
    [(:char-ci #\Y) (token-y)]
    [(eof) (token-eof)]))
(define lex
  (lexer
    [(:+ (:or #\space #\tab)) (lex input-port)]
    [(:+ #\newline) (token-newline)]
    [#\# (token-hashtag)]
    [#\: (token-colon)]
    [db/l (token-db)]
    [mnemonic/l (token-mnemonic lexeme)]
    [identifier/l (token-identifier lexeme)]
    [#\= (token-equals)]
    [label/l (token-label lexeme)]
    [(:+ numeric) (token-number (string->number lexeme))]
    [(:: #\$ (:+ hex-digit)) (token-number (string->number (substring lexeme 1) 16))]
    [#\( (token-lparen)]
    [#\) (token-rparen)]
    [#\, (token-comma)]
    [(:char-ci #\A) (token-a)]
    [(:char-ci #\X) (token-x)]
    [(:char-ci #\Y) (token-y)]
    [(eof) (token-eof)]))

(define token-list (make-parameter #f))

(define (peek-token) (car (token-list)))
(define (peek-tokens x) (take (token-list) x))
(define (read-token) (begin0 (car (token-list))
                             (token-list (cdr (token-list)))))
(define (read-tokens x) (begin0 (take (token-list) x)
                                (token-list (cdr (token-list) x))))

(define (stream-take-while pred? s)
  (cond
    [(stream-empty? s) empty-stream]
    [(pred? (stream-first s)) (stream-cons (stream-first s)
                                           (stream-take-while pred? (stream-rest s)))]
    [else empty-stream]))


(define-syntax (with-tokens-from stx)
  (syntax-case stx ()
    [(with-tokens-from (input lexer end-token) body ...)
     #`(parameterize ([token-list (for/list ([token (in-producer (λ () (lexer input)))]
                                          #:final (symbol=? (token-name token) end-token))
                               token)])
         body ...)]))

(define eof/t? (compose (symbol=? _ 'eof) token-name))
(define identifier/t? (compose (symbol=? _ 'identifier) token-name))
(define equals/t? (compose (symbol=? _ 'equals) token-name))
(define number/t? (compose (symbol=? _ 'number) token-name))
(define newline/t? (compose (symbol=? _ 'newline) token-name))
(define db/t? (compose (symbol=? _ 'db) token-name))
(define mnemonic/t? (compose (symbol=? _ 'mnemonic) token-name))
(define label/t? (compose (symbol=? _ 'label) token-name))
(define hashtag/t? (compose (symbol=? _ 'hashtag) token-name))
(define comma/t? (compose (symbol=? _ 'comma) token-name))
(define x/t? (compose (symbol=? _ 'x) token-name))
(define y/t? (compose (symbol=? _ 'y) token-name))
(define lparen/t? (compose (symbol=? _ 'lparen) token-name))
(define rparen/t? (compose (symbol=? _ 'rparen) token-name))
