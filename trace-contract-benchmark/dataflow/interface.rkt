#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide language<%>
         monotone-framework<%>
         analysis<%>)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require racket/class)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; interfaces

(define language<%>
  (interface ()
    ;; Term → Label
    ;; Returns the label of the initial control-flow block.
    init

    ;; Term → [Set Label]
    ;; Returns the set of labels of the final control-flow blocks.
    final

    ;; Term → [Set Term]
    ;; Returns the set of basic blocks.
    blocks

    ;; Term → [Set Label]
    ;; Returns the set of labels.
    labels

    ;; Term → [Set [Pair Label Label]]
    ;; Returns a set of pairs representing the flow between basic blocks.
    flow))

(define monotone-framework<%>
  (interface (language<%>)
    ;; Complete-Lattice
    ;; A property space that is a complete lattice satisfying the ascending
    ;; chain condition.
    L

    ;; [Set [Pair Label Label]]
    ;; The flow of a program under analysis.
    F

    ;; [Set Label]
    ;; The set of labels where the extremal value applies.
    E

    ;; L
    ;; An element of the property space initialized for the extremal labels.
    ι

    ;; Label L → L
    ;; Computes the transfer function for the block at the given label.
    f))

(define analysis<%>
  (interface ()
    ;; [Hash Label L]
    ;; Maps labels to facts true at the entry of the associated block.
    entry-facts

    ;; [Hash Label L]
    ;; Maps labels to facts true at the exit of the associated block.
    exit-facts))
