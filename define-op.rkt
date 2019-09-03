#lang racket

(require lens/applicable)
(require "processor.rkt")
(require "utility.rkt")
(require "mode.rkt")

(define op-λ-vector (make-vector 255))

(define p (Processor #:A 1 #:MEM #(#xA9 #x2A)))

(define (fetch-op processor)
  (lens-view (lens-compose (vector-ref-lens 0) MEM) processor))

(define (define-op name proc #:imp [imp #f] #:acc [acc #f] #:imm [imm #f] #:zp [zp #f] #:zpx [zpx #f] #:zpy [zpy #f]
                             #:abs [abs #f] #:absx [absx #f] #:absy [absy #f] #:indx [indx #f] #:indy [indy #f])
  (for ([hex (list imp acc imm zp zpx zpy abs absx absy indx indy)]
        [mode (list proc mode-acc mode-imm mode-zp mode-zpx mode-zpy mode-abs mode-absx mode-absy mode-indx mode-indy)]
        #:when (not (false? hex)))
    (vector-set! op-λ-vector hex (proc mode))))

(define-op 'ADC #:imm #x69 #:zp #x65 #:zpx #x75 #:abs #x6D #:absx #x7D #:absy #x79 #:indx #x61 #:indy #x71
 (λ (mode)
    (λ (processor)
       (define M (mode processor))
       (lens-set A processor (+ (A processor) (M processor) (C processor))))))
 
((vector-ref op-λ-vector #x69) p)

;(define (emulate processor)
;  (case (fetch-op processor)
;    [(#xA9) (define M (mode-imm processor))
;            #t]))
