#lang racket
(require fancy-app)
(define token-stream (make-parameter #f))

(define (peek-token) (stream-first (token-stream)))
(define (peek-tokens x) (stream-take (token-stream) x))
(define (read-token) (begin0 (stream-first (token-stream))
                             (token-stream (stream-rest (token-stream)))))
(define (read-tokens x) (begin0 (stream-take (token-stream) x)
                                (token-stream (stream-tail (token-stream) x))))

(define in (open-input-file "source.asm"))

(define (stream-take-while pred? s)
  (cond
    [(stream-empty? s) empty-stream]
    [(pred? (stream-first s)) (stream-cons (stream-first s)
                                           (stream-take-while pred? (stream-rest s)))]
    [else empty-stream]))


(define-syntax (with-tokens-from stx)
  (syntax-case stx ()
    [(with-tokens-from (input lexer end-token) body ...)
     #`(parameterize ([token-stream (for/stream ([token (in-producer (λ () (lexer input)))]
                                          #:break (symbol=? (token-name token) end-token))
                               token)])
         body ...)]))

(define eof/t? (compose (symbol=? _ 'eof) token-name))
(define identifier/t? (compose (symbol=? _ 'identifier) token-name))
(define equals/t? (compose (symbol=? _ 'equals) token-name))
(define number/t? (compose (symbol=? _ 'number) token-name))
(define newline/t? (compose (symbol=? _ 'newline) token-name))

(define (newline*)
  (when (newline/t? (peek-token))
    (read-token)
    (newline*)))

(define (line/p ast)
  (match (peek-token)
    [(? identifier/t? name) (identifier/p (read-token) ast)]
    [(? newline/t?) (newline*)
                    (line/p ast)]
    [else ast]))

(define (identifier/p name ast)
  (match (stream->list (peek-tokens 2))
    [(list (? equals/t?) (or (? number/t? value)
                             (? identifier/t? value))) (match (stream->list (read-tokens 2))
                                                         [(list (? equals/t?) (or (? number/t? value)
                                                                                  (? identifier/t? value))) (line/p (Equals name value))])]))

(define (start in)
  (with-tokens-from (in lex 'eof) 
    (line/p '())))


(start in)
