#lang racket
(require lens)

(struct/lens Processor (A X Y Z N PC MEM) #:transparent)

(define-syntax Processor-copy
  (λ (stx)
     (syntax-case stx ()
       [(_ p [field-name new-val] ...) #'(struct-copy Processor p [field-name new-val] ...)])))

(define flag-set? (compose not false?))

(define p (Processor 0 0 0 #t #f 0 (vector #xA9 #x00
                                           #xA2 #x00
                                           #xF0 #x02
                                           #xA2 #x2A
                                           #xA9 #x01)))

(define (first-operand processor)
  (vector-ref (Processor-MEM processor) (add1 (Processor-PC processor))))

(define twos-complement-negative? (curryr > 127))

(define (LDA-IMM processor)
  (define PC (Processor-PC processor))
  (define MEM (Processor-MEM processor))
  (define RAND (vector-ref MEM (+ PC 1)))
  (emulate (Processor-copy processor
                           [A RAND]
                           [PC (+ PC 2)]
                           [N (twos-complement-negative? RAND)]
                           [Z (= RAND 0)])))

(define (LDX-IMM processor)
  (define PC (Processor-PC processor))
  (define MEM (Processor-MEM processor))
  (define RAND (vector-ref MEM (+ PC 1)))
  (emulate (Processor-copy processor
                           [X RAND]
                           [PC (+ PC 2)]
                           [N (twos-complement-negative? RAND)]
                           [Z (= RAND 0)])))

(define (BEQ processor)
  (define PC (Processor-PC processor))
  (define MEM (Processor-MEM processor))
  (define offset (vector-ref MEM (+ PC 1)))
  (define Z (Processor-Z processor))
  (emulate (Processor-copy processor
                           [PC (if (flag-set? Z)
                                 (if (< offset 127)
                                   (+ PC offset 2)
                                   (+ PC (- offset 256) 2))
                                 (+ PC 2))])) )

(define (emulate processor)
  (if (>= (Processor-PC processor)
          (vector-length (Processor-MEM processor)))
    processor
    (match processor
      [(struct* Processor ([A A] [X X] [Y Y]
                                 [Z Z] [N N]
                                 [MEM MEM]
                                 [PC PC]))
       (case (vector-ref MEM PC)
         [(#xA9) (LDA-IMM processor)]
         [(#xA2) (LDX-IMM processor)]
         [(#xF0) (BEQ processor)])])))

(emulate p)
