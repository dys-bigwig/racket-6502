#lang rackjure
;(require bitsyntax)
;(require file/sha1)
;(require binaryio)
(require threading)
(require sugar)
(require json)

(define 16KB #x4000)
(define 8KB  #x2000)
(define iNES-identifier (bytes #x4e #x45 #x53 #x1a))

(define hex-string->number
  (λ~> (substring 1)
       (string->number 16)))

(define OPS
  (let ([v (make-vector 256 #f)])
    (for ([op (read-json (open-input-file "6502_instructions.json"))])
      (match-let* ([(hash-table ('opcode opcode)
                                ('name name)
                                ('mode mode)
                                ('bytes bytes)) op])
        (vector-set! v (hex-string->number opcode) (vector opcode name mode bytes))))
    v))

(for ([op OPS])
  (displayln op))

;; CONSTANTS ;;
(define 1*PRG-BANK #x4000)
(define 1*CHR-BANK #x2000)
(define OPCODE 0)
(define NAME 1)
(define MODE 2)
(define BYTES 3)

(define (valid-ines-header? h)
  (for/and ([b (in-bytes h)]
            [n (in-bytes iNES-identifier)])
    (equal? b b)))

(define (filesize-matches-header? file-size
                                  prg-size
                                  chr-size)
  (if (= (+ prg-size chr-size 16)
         file-size)
    "FILESIZE MATCHES HEADER"
    "FILESIZE DOES NOT MATCH HEADER"))

(define (main rom-path)
  (define ROM (open-input-file rom-path #:mode 'binary))
  (unless (valid-ines-header? (read-bytes 4 ROM))
    (error "error: no iNES header")
    (exit))
  (define PRG-SIZE (* (read-byte ROM) 16KB))
  (define CHR-SIZE (* (read-byte ROM) 8KB))
  (unless (filesize-matches-header? (file-size rom-path)
                                    PRG-SIZE
                                    CHR-SIZE)
    (error "error: filesize does not match header")
    (exit)))




(main "mario.nes")



