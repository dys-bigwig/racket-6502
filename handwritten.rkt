#lang racket
(require fancy-app
         "lexer/lex.rkt")

(struct Instruction (name operand) #:transparent)
(struct Operand (value mode index) #:transparent)
(struct Label (name) #:transparent)
(struct Identifier (name) #:transparent)
(struct Equals (name value) #:transparent)
(struct Db (bytes) #:transparent)

(define in (open-input-string "LDA (22),y"))

#| number |#

(define (number*)
  (read-token)
  (match (peek-token)
    [(? number/t? n) (cons (read-token)
                           (number*))]
    [else '()]))

(define (number+)
  (match (peek-token)
    [(? number/t? n) (cons (read-token)
                           (number*))]
    [else (error "expected number, recieved" (peek-token))]))

#| db |#

(define (db)
  (match (peek-token)
    [(? number/t? n) (Db (number+))]
    [else (error "expected number, recieved" (peek-token))]))

#| identifier |#

(define (identifier)
  (define name (read-token))
  (match (token-list) 
    [`(,(? equals/t?) ,(? number/t?)) (read-token)
                                      (Equals name (read-token))]))

(define (newline*)
  (when (newline/t? (peek-token))
    (begin (read-token)
           (newline*))))

#| label |#

(define (label)
  (begin0 (read-token)
          (newline*)))

#| mnemonic |#

(define (mnemonic)
  (read-token)
  (operand))


(define (operand)
  (match (token-list)
    [`(,(? hashtag/t?) ,(? number/t?) . ,rest)
      (begin (read-token)
             (Operand (read-token) 'IMM #f))]

    [`(,(? number/t?) ,(? comma/t?) ,(? x/t?) . ,rest)
      (define n (token-value (read-token)))
      (read-token)
      (define mode (if (> n #xFF) 'ABS 'ZP))
      (Operand n mode 'X)]

    [`(,(? number/t?) ,(? comma/t?) ,(? y/t?) . ,rest)
      (define n (token-value (read-token)))
      (read-token)
      (define mode (if (> n #xFF) 'ABS 'ZP))
      (Operand n mode 'Y)]

    [`(,(? number/t?) . ,rest)
      (define n (token-value (read-token)))
      (define mode (if (> n #xFF) 'ABS 'ZP))
      (Operand n mode #f)]

    [`(,(? lparen/t?) ,(? number/t?) ,(? rparen/t?) ,(? comma/t?) ,(? y/t?) . ,rest)
      (read-token)
      (define n (token-value (read-token)))
      (begin0 (Operand n 'IND 'Y)
      (read-token)
      (read-token)
      (read-token))]

    [`(,(? lparen/t?) ,(? number/t?) ,(? comma/t?) ,(? x/t?) ,(? rparen/t?) . ,rest)
      (read-token)
      (define n (token-value (read-token)))
      (begin0 (Operand n 'IND 'X)
      (read-token)
      (read-token)
      (read-token))]

    [`(,(? lparen/t?) ,(? number/t?) ,(? rparen/t?) . ,rest)
      (read-token)
      (define n (token-value (read-token)))
      (begin0 (Operand n 'IND #f)
              (read-token))]))

#| line |#

(define (line ast symbol-table)
  (match (peek-token)
    [(? db/t?) (db)]
    [(? identifier/t?) (identifier)]
    [(? label/t?) (label)]
    [(? mnemonic/t?) (mnemonic)]))

#| start |#

(define (start in)
  (with-tokens-from (in lex 'eof)
                    (line '() (hash))))

(start in)
