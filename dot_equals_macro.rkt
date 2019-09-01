#lang racket
(require racket/stxparam
         (for-syntax syntax/parse racket/base))

(require lens)

(struct/lens Processor (A) #:transparent)

(define A Processor-A-lens)

(define-syntax-parameter processor
  (lambda (stx)
    (raise-syntax-error #f "Use of processor id outside of valid context" stx)))

(define-syntax (:= stx)
  (syntax-parse stx 
    [(_ dst src)
     (define (transform expr)
       (for/list ([e expr])
         (if (member e '(A X Y C Z I D B V N))
           (list 'lens-view e 'processor)
           e)))
     (with-syntax ([new-src (datum->syntax #'src (transform (syntax->datum #'src)))])
     #'(λ (processor-p)
          (syntax-parameterize ([processor (make-rename-transformer #'processor-p)])
            (lens-set dst processor-p new-src))))]))

((:= A (+ A A)) (Processor 2))
