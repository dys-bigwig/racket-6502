#lang racket
(require lens)
(require "ops.rkt")
(require data/pvector)
;(make-pvector 65536 0) ;switch to pvectors?

#| UTILITY |#
;------------

(struct/lens Processor (A X Y Z N PC MEM) #:transparent)

(define-syntax Processor-copy
  (λ (stx)
     (syntax-case stx ()
       [(_ p [field-name new-val] ...) #'(struct-copy Processor p [field-name new-val] ...)])))

(define twos-complement-negative? (curryr > 127))

(define flag-set? (compose not false?))

(define (bytes->addr b1 b2)
  (bitwise-ior (arithmetic-shift b1 8) b2))

(define (byte->twos-complement b)
  (if (< b 127)
    b
    (- b 256)))

#| STRUCT |#
;-----------

(define p (Processor #x2A 0 0 #t #f 0 (vector-immutable #x8D #x00 #x03 #x00 #x00)))

(define p* (Processor 0 0 0 #t #f 0 (vector #xA9 #x00
                                           #xA2 #x00
                                           #xF0 #x02
                                           #xA2 #x2A
                                           #xA9 #x01)))

(define (first-operand processor)
  (vector-ref (Processor-MEM processor) (add1 (Processor-PC processor))))

(define (second-operand processor)
  (vector-ref (Processor-MEM processor) (+ 2 (Processor-PC processor))))

#| OPS |#
;--------

(define (LOAD register-lens mode processor)
  (lens-set register-lens processor (mode (first-operand processor))))

(define (STORE register-lens mode processor)
  (define MEM (Processor-MEM processor))
  (define PC (Processor-PC processor))
  (define len (OP-LEN (vector-ref MEM PC)))
  (define address (mode (if (= len 1)
                          (first-operand processor)
                          (bytes->addr (first-operand processor)
                                       (second-operand processor)))))
  (lens-set (lens-compose (vector-ref-lens address) Processor-MEM-lens) processor (lens-view register-lens processor)))

(define (LDA-IMM processor)
  (define PC (Processor-PC processor))
  (define MEM (Processor-MEM processor))
  (define RAND (vector-ref MEM (+ PC 1)))
  (Processor-copy processor
                          [A RAND]
                          [PC (+ PC 2)]
                          [N (twos-complement-negative? RAND)]
                          [Z (= RAND 0)]))

(define (LDA-ZP processor)
  (define PC (Processor-PC processor))
  (define MEM (Processor-MEM processor))
  (define RAND (vector-ref MEM (+ PC 1)))
  (define address (+ #x0000 RAND))
  (Processor-copy processor
                          [A (vector-ref MEM address)]
                          [PC (+ PC 1)]
                          [N (twos-complement-negative? RAND)]
                          [Z (= RAND 0)]))

(define (LDA-ZP-X processor)
  (define PC (Processor-PC processor))
  (define MEM (Processor-MEM processor))
  (define RAND (vector-ref MEM (+ PC 1)))
  (define X (Processor-X processor))
  (define address (+ #x0000 X))
  (Processor-copy processor
                          [A (vector-ref MEM address)]
                          [PC (+ PC 1)]
                          [N (twos-complement-negative? RAND)]
                          [Z (= RAND 0)]))

(define (LDA-ZP-Y processor)
  (define PC (Processor-PC processor))
  (define MEM (Processor-MEM processor))
  (define RAND (vector-ref MEM (+ PC 1)))
  (define Y (Processor-Y processor))
  (define address (+ #x0000 Y))
  (Processor-copy processor
                          [A (vector-ref MEM address)]
                          [PC (+ PC 1)]
                          [N (twos-complement-negative? RAND)]
                          [Z (= RAND 0)]))

(define (LDA-ABS processor)
  (define PC (Processor-PC processor))
  (define MEM (Processor-MEM processor))
  (define RAND (vector-ref MEM (+ PC 1)))
  (Processor-copy processor
                          [A (vector-ref MEM (vector-ref MEM RAND))]
                          [PC (+ PC 1)]
                          [N (twos-complement-negative? RAND)]
                          [Z (= RAND 0)]))

(define (LDA-ABS-X processor)
  (define PC (Processor-PC processor))
  (define MEM (Processor-MEM processor))
  (define RAND (vector-ref MEM (+ PC 1)))
  (define X (Processor-X processor))
  (define address (+ RAND X))
  (Processor-copy processor
                          [A (vector-ref MEM address)]
                          [PC (+ PC 1)]
                          [N (twos-complement-negative? RAND)]
                          [Z (= RAND 0)]))

(define (LDA-ABS-Y processor)
  (define PC (Processor-PC processor))
  (define MEM (Processor-MEM processor))
  (define RAND (vector-ref MEM (+ PC 1)))
  (define Y (Processor-Y processor))
  (define address (+ RAND Y))
  (Processor-copy processor
                          [A (vector-ref MEM address)]
                          [PC (+ PC 1)]
                          [N (twos-complement-negative? RAND)]
                          [Z (= RAND 0)]))

(define (LDA-IND-X processor)
  (define PC (Processor-PC processor))
  (define MEM (Processor-MEM processor))
  (define RAND (vector-ref MEM (+ PC 1)))
  (define X (Processor-X processor))
  (define pointer (vector-ref MEM (+ #x0000 RAND X)))
  (Processor-copy processor
                          [A (vector-ref MEM pointer)]
                          [PC (+ PC 1)]
                          [N (twos-complement-negative? RAND)]
                          [Z (= RAND 0)]))

(define (LDA-IND-Y processor)
  (define PC (Processor-PC processor))
  (define MEM (Processor-MEM processor))
  (define RAND (vector-ref MEM (+ PC 1)))
  (define Y (Processor-Y processor))
  (define pointer (+ Y (vector-ref MEM (+ #x0000 RAND))))
  (Processor-copy processor
                          [A (vector-ref MEM pointer)]
                          [PC (+ PC 1)]
                          [N (twos-complement-negative? RAND)]
                          [Z (= RAND 0)]))

(define (LDX-IMM processor)
  (define PC (Processor-PC processor))
  (define MEM (Processor-MEM processor))
  (define RAND (vector-ref MEM (+ PC 1)))
  (Processor-copy processor
                          [X RAND]
                          [PC (+ PC 2)]
                          [N (twos-complement-negative? RAND)]
                          [Z (= RAND 0)]))

(define (BEQ processor)
  (define PC (Processor-PC processor))
  (define MEM (Processor-MEM processor))
  (define offset (byte->twos-complement (vector-ref MEM (+ PC 1))))
  (define Z (Processor-Z processor))
  (Processor-copy processor
                          [PC (if (flag-set? Z)
                                (+ PC offset 2)
                                (+ PC 2))]) )
#| EMULATE |#
;------------

(define (emulate processor)
  (cond
    [(>= (Processor-PC processor)
         (vector-length (Processor-MEM processor)))
     processor]
    [else
      (match-define
        (struct* Processor ([A A] [X X] [Y Y] [Z Z] [N N] [MEM MEM] [PC PC]))
        processor)
      (case (vector-ref MEM PC)
        [(#xA9) (LOAD Processor-A-lens identity processor)]
        [(#x8D) (STORE Processor-A-lens identity processor)]
        [(#xF0) (BEQ processor)])]))

(emulate p)
