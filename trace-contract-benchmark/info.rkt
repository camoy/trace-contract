#lang info

;; general

(define name "trace-contract")
(define collection "trace-contract")

(define compile-omit-paths '("util/modifier"))
(define test-omit-paths '("util/modifier"))

;; dependencies

(define deps
  '("data-frame"
    "sawzall-lib"
    "scribble-abbrevs"
    "threading-lib"
    "whereis"
    "syntax-sloc"
    "automata-lib"
    "base"
    "contract-etc-lib"
    "data-lib"
    "draw-lib"
    "gui-lib"
    "htdp-lib"
    "math-lib"
    "pict-lib"
    "plot-gui-lib"
    "plot-lib"
    "r6rs-lib"
    "rackunit-lib"
    "redex-etc"
    "redex-lib"
    "scribble-lib"
    "stream-etc"
    "trace-contract-lib"
    ))

(define build-deps
  '("at-exp-lib"
    "rackunit-typed"
    ))
