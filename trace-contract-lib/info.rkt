#lang info

;; general

(define name "trace-contract")
(define collection "trace-contract")
(define pkg-desc "Core implementation of trace-contract (no tests or documentation)")
(define version "0.0")
(define pkg-authors '(camoy))

;; dependencies

(define deps
  '("automata-lib"
    "meta"
    "base"
    "ee-lib"))

(define build-deps
  '("chk-lib"))
