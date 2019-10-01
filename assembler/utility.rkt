#lang racket
(provide (all-defined-out))

(define (get-in h . ks)
  (let get ([h h] [ks ks])
    (cond
      [(null? ks) h]
      [else
        (define k (car ks))
        (define h^ (hash-ref h k))
        (get h^ (rest ks))])))

(define (addr-lobyte addr)
  (bitwise-and addr #xFF))
(define (addr-hibyte addr)
  (bitwise-and (arithmetic-shift addr -8) #xFF))
(define (string-cdr s)
  (substring s 1 (string-length s)))
