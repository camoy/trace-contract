#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require racket/runtime-path)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; constants

(define ITERS 20)
(define TIMEOUT-MIN 2)
(define TIMEOUT-SECS (* TIMEOUT-MIN 60))

(define LEVELS '("base" "noop" "check"))

(define CTC-PATHS '("trace-contract.rkti"))
(define GAME-CTC-PATHS
  '("common/player-trace-contract.rkti"
    "common/referee-trace-contract.rkti"))
(define DRAW-CTC-PATHS
  '("../util/modifier/draw/trace-contract.rkti"))

(define BENCHMARKS
  `(["dataflow" ,LEVELS ,CTC-PATHS]
    ["dungeon"  ,LEVELS ,CTC-PATHS]
    ["fish"     ,LEVELS ,GAME-CTC-PATHS]
    ["future"   ,LEVELS ,DRAW-CTC-PATHS]
    ["jpeg"     ,LEVELS ,CTC-PATHS]
    ["lnm"      ,LEVELS ,DRAW-CTC-PATHS]
    ["memory"   ,LEVELS ,CTC-PATHS]
    ["tetris"   ,LEVELS ,DRAW-CTC-PATHS]
    ["ticket"   ,LEVELS ,GAME-CTC-PATHS]))

(define MODIFIERS
  `(["modifier/typed-racket-more/draw.rkt" typed/racket/draw #f]
    ["modifier/draw/main.rkt" racket/draw "modifier/draw/support"]
    ["modifier/gui/cocoa-canvas.rkt" mred/private/wx/cocoa/canvas #f]
    ["modifier/gui/gtk-canvas.rkt" mred/private/wx/gtk/canvas #f]))

(define COLUMNS
  '("sample"
    "benchmark"
    "level"
    "cpu"
    "real"
    "gc"
    "projections"
    "collections"))

(define LVL-VAR "TRACE_CONTRACT_LEVEL")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; computed constants

(define-runtime-path PARENT "..")

(define (exe benchmark)
  (build-path PARENT benchmark "main.rkt"))

(define BENCHMARK-PATHS
  (map (compose exe car) BENCHMARKS))
