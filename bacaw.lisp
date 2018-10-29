;;; -*- mode: Lisp; coding: utf-8-unix -*-

(in-package :repl)

(require "isa")

(define-isa bacaw-isa
    (:NOP (0 0))
  (:NOT (0 1))
  (:OR (0 2))
  (:XOR (0 3))
  (:AND (0 4))
  (:BSL ( 0 5))
  (:INT (#x0 #x7))
  (:HALT (0 8))
  (:NEG (0 9))
  (:RTI (0 12))
  (:BSR (0 13))
  (:CLS (0 #xe))
  (:INC (#x1) :ulong)
  (:ADDI (#x2 #x1))
  (:SUBI (#x2 #x9))
  (:MULI (#x2 #x2))
  (:POWI (#x2 #x4))
  (:DIVI (#x2 #xa))
  (:CONVI (#x2 #xb))
  (:CMPI (#x2 0))
  (:ADDF (#x4 #x1))
  (:SUBF (#x4 #x9))
  (:MULF (#x4 #x2))
  (:DIVF (#x4 #xa))
  (:CMPF (#x4 0))
  (:CEIL (#x4 #x6))
  (:LOAD (#x5) :long)
  (:POP (#x6))
  (:CALL (#x7 #x7) :ulong)
  (:RET (#x7 #xc))
  (:CIE (#x7 #x0))
  (:SIE (#x7 #x8))
  (:RESET (#x7 #x1))
  (:MOV (#x8))
  (:DEC (#x9) :ulong)
  (:STORE (#xD) :long)
  (:PUSH (#xE)))
