#lang info

;; general

(define name "trace-contract")
(define collection "trace-contract")
(define scribblings
  '(["scribblings/trace-contract.scrbl" ()]))

;; dependencies

(define deps
  '("base"
    "trace-contract-lib"))

(define implies
  '("trace-contract-lib"))

(define build-deps
  '("chk-lib"
    "racket-doc"
    "sandbox-lib"
    "scribble-lib"))
