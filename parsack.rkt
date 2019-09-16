#lang racket
(require "instruction.rkt")
(require "lexer/lex.rkt")
(require lens)
(require fancy-app)
(require parser-tools/lex)
(require megaparsack megaparsack/parser-tools/lex)
(require data/monad)
(require data/functor)
(require data/applicative)
(require fancy-app)
(require data/either)
(provide parse-file)

(define symbol-table (make-parameter (hash)))

(define number+/p
  (many+/p (token/p 'number)))

(define newline/p
  (token/p 'newline))

(define x/p
  (token/p 'x))

(define y/p
  (token/p 'y))

(define number/p
  (token/p 'number))

(define comma/p
  (token/p 'comma))

(define lparen/p
  (token/p 'lparen))

(define rparen/p
  (token/p 'rparen))

(define mnemonic/p
  (token/p 'mnemonic))

(define identifier/p
  (token/p 'identifier))

(define equals/p
  (token/p 'equals))

(define label/p
  (do 
    [name <- (token/p 'label)]
    (or/p (try/p newline/p)
          (try/p void/p))
    (pure (Label name))))

(define immediate/p
  (do
    [token/p 'hashtag]
    [n <- (token/p 'number)]
    (pure (Operand n 'imm #f))))

(define zp/p
  (do
    [n <- (guard/p number/p
                   (<= _ #xFF))]
    (pure (Operand n 'ZP #f))))

(define zp-x/p
  (do
    [n <- (guard/p number/p (<= _ #xFF))]
    comma/p
    x/p
    (pure (Operand n 'ZP 'X))))

(define zp-y/p
  (do
    [n <- (guard/p number/p (<= _ #xFF))]
    comma/p
    y/p
    (pure (Operand n 'ZP 'Y))))

(define abs/p
  (do
    [n <- (guard/p number/p
                   (> _ #xFF))]
    (pure (Operand n 'ABS #f))))

(define abs-x/p
  (do
    [n <- (guard/p number/p (> _ #xFF))]
    comma/p
    x/p
    (pure (Operand n 'ABS 'X))))

(define abs-y/p
  (do
    [n <- (guard/p number/p (> _ #xFF))]
    comma/p
    y/p
    (pure (Operand n 'ABS 'Y))))

(define ind/p
  (do
    lparen/p
    [val <- (or/p (try/p (guard/p number/p (> _ #xFF)))
                  (try/p identifier/p))]
    rparen/p
    (pure (Operand val 'IND #f))))

(define ind-x/p
  (do
    lparen/p
    [val <- (or/p (try/p (guard/p number/p (> _ #xFF)))
                  (try/p identifier/p))]   
    comma/p
    x/p
    rparen/p
    (pure (Operand val 'IND 'X))))

(define ind-y/p
  (do
    lparen/p
    [val <- (or/p (try/p (guard/p number/p (> _ #xFF)))
                  (try/p identifier/p))]
    rparen/p
    comma/p
    y/p
    (pure (Operand val 'IND 'Y))))

(define operand/p
  (do
    [operand <- (or/p (try/p immediate/p)
                      (try/p zp-x/p)
                      (try/p zp-y/p)
                      (try/p abs-x/p)
                      (try/p abs-y/p)
                      (try/p zp/p)
                      (try/p abs/p)
                      (try/p ind-x/p)
                      (try/p ind-y/p)
                      (try/p ind/p)
                      (try/p (do [name <- identifier/p]
                                 (pure (Operand name '? #f))))
                      (do void/p        ;alternatively, 'newline
                        (pure (Operand #f 'IMP #f))))]
    (pure operand)))

(define instruction-statement/p
  (do
    [mnemonic <- mnemonic/p]
    [operand <- operand/p]
    newline/p
    (pure (Instruction (string->symbol mnemonic) (if (and (equal? mnemonic "JMP")
                                                          (equal? (Operand-mode operand) '?))
                                                   (lens-set Operand-mode-lens operand 'ABS)
                                                   operand)))))

(define db/p
  (do
    [token/p 'db]
    [ns <- number+/p]
    newline/p
    (pure (Db ns))))

(define assignment/p
  (do
    [name <- identifier/p]
    equals/p
    [value <- number/p]
    newline/p
    (pure (Assignment name value))))

(define statement/p
  (do
    (or/p (try/p instruction-statement/p)
          (try/p db/p)
          (try/p label/p)
          (try/p assignment/p))))

(define program/p
  (many+/p statement/p))

(define (get-token-name token)
  (cond
    [(symbol? token) token]
    [(position-token? token) (get-token-name (position-token-token token))]
    [(token? token) (token-name token)]
    [else token]))

(define (flatten-token token)
  (cond
    [(position-token? token)
     (cond
       [(position-token? (position-token-token token)) (flatten-token (position-token-token token))]
       [else token])]))

(define (parse-file path)
  (call-with-input-file path
    (λ (in) (parse-result!
               (parse-tokens program/p
                             (for/list ([token (in-producer (thunk (lex-src in)))]
                                        #:final (symbol=? (get-token-name token) 'eof))
                               (flatten-token token)))))))

(define (parse-string str)
  (call-with-input-string str
    (λ (in) (parse-result!
               (parse-tokens program/p
                             (for/list ([token (in-producer (thunk (lex-src in)))]
                                        #:final (symbol=? (get-token-name token) 'eof))
                               (flatten-token token)))))))
