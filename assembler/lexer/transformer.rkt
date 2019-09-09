#lang racket
(require (only-in parser-tools/lex define-lex-trans))
(require (for-syntax (only-in syntax/parse syntax-parse)
                     (only-in syntax/stx stx-map)))
(provide :char-ci :string-ci :strings-ci)

(define-for-syntax (char-ci char)
  `(union ,(char-upcase char)
          ,(char-downcase char)))

(define-for-syntax (string-ci str)
  `(concatenation ,@(map char-ci (string->list (syntax->datum str)))))

(define-lex-trans :char-ci (λ (stx)
                             (syntax-parse stx
                               [(_ char) (datum->syntax stx (char-ci (syntax->datum #'char)))])))

(define-lex-trans :string-ci (λ (stx)
                               (syntax-parse stx
                                 [(_ str) #`(concatenation #,@(stx-map char-ci #'str))])))

(define-lex-trans :strings-ci (λ (stx)
                                 (syntax-parse stx
                                   [(_ ss ...) #`(union #,@(stx-map string-ci #'(ss ...)))])))
