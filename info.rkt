#lang info

;; General

(define collection "trace-contract")
(define pkg-desc "Contracts over traces of values.")
(define version "0.0")
(define pkg-authors '(camoy))
(define scribblings '(("scribblings/trace-contract.scrbl" ())))

;; Dependencies

(define deps
  '("base"
    "ee-lib"))

(define build-deps
  '("chk-lib"
    "racket-doc"
    "sandbox-lib"
    "scribble-lib"))
