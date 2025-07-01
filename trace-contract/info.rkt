#lang info

;; general

(define name "trace-contract")
(define collection "trace-contract")
(define scribblings
  '(["scribblings/trace-contract.scrbl" ()]))

;; dependencies

(define deps
  '("logic-lib"
    "automata-lib"
    "contract-etc-lib"
    "data-lib"
    "graph-lib"
    "redex-etc"
    "redex-lib"
    "redex-pict-lib"
    "stream-etc"
    "base"
    "trace-contract-lib"))

(define implies
  '("trace-contract-lib"))

(define build-deps
  '("rackunit-lib"
    "chk-lib"
    "racket-doc"
    "sandbox-lib"
    "scribble-lib"))
