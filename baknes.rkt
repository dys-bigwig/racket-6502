#lang rackjure
;(require bitsyntax)
(require file/sha1)
(require csv-reading)
(require binaryio)

;for/vector in-input-port-bytes

;hex ;mnemonic ;mode ;length

;(define OP-VEC
;  (list->vector (map list->vector (csv-map (λ (ln)
;                                              (if (member "" ln)
;                                                '()
;                                                (map (λ (elem)
;                                                        (or (string->number elem)
;                                                            elem))
;                                                     ln)))(file->string "6502opswip.csv")))))


;; CONSTANTS ;;
(define 1*PRG-BANK #x4000)
(define 1*CHR-BANK #x2000)

(define (rom->bytes rom)
  (file->bytes rom #:mode 'binary))

(define (rom->header rom)
  {'NES-HEADER (subbytes  rom 0 4)
  'PRG-BANKS  (bytes-ref rom 4)
  'CHR-BANKS  (bytes-ref rom 5)
  'FLAGS-6    (bytes-ref rom 6)
  'FLAGS-7    (bytes-ref rom 7)
  'FLAGS-8    (bytes-ref rom 8)
  'FLAGS-9    (bytes-ref rom 9)
  'FLAGS-10   (bytes-ref rom 10)
  '11-15      (subbytes  rom 11 16)})

(define (lookup op)
  (let ([val (OP-VEC op)])
    (if (or (equal? val #())
            (false? val))
      (vector "" "UNDEFINED" "" 1 "")
      val)))


(define (op->str op i byte2 byte3)
  (case (op 2)
    [("REL") (format "B0_~a:\t~a B0_~a\n" (number->string i 16)
                     (str (op 1))
                     (number->string (+ i
                                        (if (> byte2 127)
                                          (- byte2 256)
                                          byte2)
                                        2)
                                     16))]
    [else (string-append (str op) "\n")]))




(define (mainold rom-path)
  (define ROM (rom->bytes rom-path))
  (define HEADER (rom->header ROM))
  (define PRG-SIZE (* (HEADER 'PRG-BANKS) 1*PRG-BANK))
  (define CHR-SIZE (* (HEADER 'CHR-BANKS) 1*CHR-BANK))
  (define PRG-ROM (subbytes ROM 16 (+ PRG-SIZE 16)))
  (let loop ([i 0] [res '()])
    (if (= i (- PRG-SIZE 3))
      (reverse res) 
      (let* ([ins (bytes-ref PRG-ROM i)]
             [op (lookup ins)]
             [len (op 3)])
        (loop (+ len i)
              (cons (op->str op
                             i
                             (bytes-ref PRG-ROM (+ i 1))
                             (bytes-ref PRG-ROM (+ i 2)))
                    res))))))
(define 16KB #x4000)
(define 8KB  #x2000)

(define (valid-ines-header? h)
  (for/and ([b (in-bytes h)]
            [n (in-bytes (bytes #x4e #x45 #x53 #x1a))])
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



;for/vector in-input-port-bytes


(main "mario.nes")


(define (rom->bytes rom)
  (file->bytes rom #:mode 'binary))

(define (rom->header rom)
  {'NES-HEADER (subbytes  rom 0 4)
  'PRG-BANKS  (bytes-ref rom 4)
  'CHR-BANKS  (bytes-ref rom 5)
  'FLAGS-6    (bytes-ref rom 6)
  'FLAGS-7    (bytes-ref rom 7)
  'FLAGS-8    (bytes-ref rom 8)
  'FLAGS-9    (bytes-ref rom 9)
  'FLAGS-10   (bytes-ref rom 10)
  '11-15      (subbytes  rom 11 16)})

(define (op->str op i byte2 byte3)
  (case (op 2)
    [("REL") (format "B0_~a:\t~a B0_~a\n" (number->string i 16)
                     (str (op 1))
                     (number->string (+ i
                                        (if (> byte2 127)
                                          (- byte2 256)
                                          byte2)
                                        2)
                                     16))]
    [else (string-append (str op) "\n")]))





;(for ([op OP-VEC])
;  (displayln op))

