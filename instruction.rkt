#lang racket
(require lens)
(provide (all-defined-out))

(struct/lens Instruction (name operand) #:transparent)
(struct/lens Operand (value mode) #:transparent)
(struct/lens Label (name) #:transparent)
(struct/lens Identifier (name) #:transparent)
(struct/lens Assignment (name value) #:transparent)
(struct/lens Db (bytes) #:transparent)
