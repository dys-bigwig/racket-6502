#lang racket
(provide (all-defined-out))

(define (bytes->addr b1 [b2 0])
  (bitwise-ior (arithmetic-shift b2 8) b1))

(define (add2 x)
  (+ x 2))
