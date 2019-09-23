#lang racket/base
(provide (all-defined-out))

(define (addr-lobyte addr)
  (bitwise-and addr #xFF))
(define (addr-hibyte addr)
  (bitwise-and (arithmetic-shift addr -8) #xFF))
(define (string-init s)
  (substring s 0 (- (string-length s) 1)))
