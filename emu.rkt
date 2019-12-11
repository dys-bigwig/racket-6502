#lang racket
(require lens)
(require "lenses.rkt")
(require "utility.rkt")
(require fancy-app)
(require data/collection)
(require data/pvector)

(define p (Processor 0 1 0 #t #f 0 (pvector #xA9 #x03 #x00 #x05 #x00 #x00)))

(define (emulate processor)
  (for/fold ([p processor])
    	    ()
    (let ([opcode (nth (Processor-MEM processor)
		       (Processor-PC processor))])
      ((case opcode
	 [(#xA9) (LDA IMM)]
	 [(#xA5) (LDA ZP)]
	 [(#xB5) (LDA ZPX)]
	 [(#xAD) (LDA ABS)]
	 [(#xBD) (LDA ABSX)]
	 [(#xB9) (LDA ABSY)]
	 [(#xA1) (LDA INDX)])
       processor))))

;not moving pc!

(emulate p)
