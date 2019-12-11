#lang racket
(require lens)
(require "utility.rkt")
(require data/collection)
(require data/pvector)

(provide (all-defined-out))

(struct/lens Processor (A X Y Z N PC MEM) #:transparent)
(define A Processor-A-lens)
(define X Processor-X-lens)
(define Y Processor-Y-lens)
(define Z Processor-Z-lens)
(define N Processor-N-lens)
(define PC Processor-PC-lens)
(define MEM Processor-MEM-lens)


(define IMM
  (make-lens (λ (processor)
		(let ([memory (Processor-MEM processor)]
		      [program-counter (Processor-PC processor)])
		  (nth memory (add1 program-counter))))
	     (λ (processor val)
		(let ([memory (Processor-MEM processor)]
		      [program-counter (Processor-PC processor)])
		  (lens-set MEM processor (set-nth memory (add1 program-counter) val))))))

(define ZP
  (make-lens (λ (processor)
		(nth (Processor-MEM processor) (nth (Processor-MEM processor) (add1 (Processor-PC processor)))))
	     (λ (processor val)
		(lens-set MEM processor (nth (lens-view MEM processor) (add1 (lens-view PC processor))) ))))

(define ZPX
  (make-lens (λ (processor)
		(let ([memory (Processor-MEM processor)]
		      [program-counter (Processor-PC processor)]
		      [x (Processor-X processor)])
		  (nth memory (+ x (nth memory (add1 program-counter))))))
	     (λ (processor val)
		(let ([memory (Processor-MEM processor)]
		      [program-counter (Processor-PC processor)]
		      [x (Processor-X processor)])
		  (lens-set MEM processor (nth memory (add1 program-counter)) )))))

(define ZPY
  (make-lens (λ (processor)
		(let ([memory (Processor-MEM processor)]
		      [program-counter (Processor-PC processor)]
		      [y (Processor-Y processor)])
		  (nth memory (+ y (nth memory (add1 program-counter))))))
	     (λ (processor val)
		(let ([memory (Processor-MEM processor)]
		      [program-counter (Processor-PC processor)]
		      [Y (Processor-Y processor)])
		  (lens-set MEM processor (nth memory (add1 program-counter)) )))))

(define ABS
  (make-lens (λ (processor)
		(let ([memory (Processor-MEM processor)]
		      [program-counter (Processor-PC processor)])
		  (nth memory (bytes->addr (nth memory (add1 program-counter))
					   (nth memory (add2 program-counter))))))
	     (λ (processor val)
		(let ([memory (Processor-MEM processor)]
		      [program-counter (Processor-PC processor)])
		  (lens-set MEM processor (set-nth memory (bytes->addr (nth memory (add1 program-counter))
								       (nth memory (add2 program-counter))) val))))))

(define ABSX
  (make-lens (λ (processor)
		(let ([memory (Processor-MEM processor)]
		      [program-counter (Processor-PC processor)]
		      [x (Processor-X processor)])
		  (nth memory (+ x (bytes->addr (nth memory (add1 program-counter))
						(nth memory (add2 program-counter)))))))
	     (λ (processor val)
		(let ([memory (Processor-MEM processor)]
		      [program-counter (Processor-PC processor)]
		      [x (Processor-X processor)])
		  (lens-set MEM processor (set-nth memory (+ x (bytes->addr (nth memory (add1 program-counter))
									    (nth memory (add2 program-counter)))) val))))))


(define ABSY
  (make-lens (λ (processor)
		(let ([memory (Processor-MEM processor)]
		      [program-counter (Processor-PC processor)]
		      [y (Processor-Y processor)])
		  (nth memory (+ y (bytes->addr (nth memory (add1 program-counter))
						(nth memory (add2 program-counter)))))))
	     (λ (processor val)
		(let ([memory (Processor-MEM processor)]
		      [program-counter (Processor-PC processor)]
		      [y (Processor-Y processor)])
		  (lens-set MEM processor (set-nth memory (+ y (bytes->addr (nth memory (add1 program-counter))
									    (nth memory (add2 program-counter)))) val))))))

(define INDX
  (make-lens (λ (processor)
		(let* ([memory (Processor-MEM processor)]
		       [program-counter (Processor-PC processor)]
		       [x (Processor-X processor)]
		       [lo-byte (nth memory (+ x (nth memory (add1 program-counter))))]
		       [hi-byte (nth memory (+ x (add1 (nth memory (add1 program-counter)))))])
		  (nth memory (bytes->addr lo-byte hi-byte))))
	     (λ (processor val)
		(let* ([memory (Processor-MEM processor)]
		       [program-counter (Processor-PC processor)]
		       [x (Processor-X processor)]
		       [lo-byte (nth memory (+ x (nth memory (add1 program-counter))))]
		       [hi-byte (nth memory (+ x (add1 (nth memory (add1 program-counter)))))])
		  (lens-set MEM processor (set-nth memory (bytes->addr lo-byte hi-byte) val))))))

(define (LDA mode)
  (λ (p)
     (lens-set A p (lens-view mode p))))

(define (LDX mode p)
  (lens-set X p (lens-view mode p)))

(define (LDY mode p)
  (lens-set Y p (lens-view mode p)))
