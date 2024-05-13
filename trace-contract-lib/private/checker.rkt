#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide (struct-out checker)
         checker-wrap-folder
         checker-update!
         checker-default-error)

(module+ private
  (provide (all-defined-out)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require racket/class
         racket/contract
         racket/hash
         racket/list
         racket/match
         racket/set
         racket/string
         racket/struct
         "clause.rkt"
         "fail.rkt"
         "logger.rkt"
         "subclause.rkt"
         "util.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; constants

(define ERROR-MSG-LINES
  '("\n  accumulator: ~a"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; data

;; `Checker-Hash` ≜ [Hash Symbol [Set Checker]]
;; Maps variables to the set of checkers that must be updated once that variable
;; has a new value collected.

;; `checker` contains the accumulator and the information needed to update it.
;;   - `acc-box` : Box, contains the accumulator
;;   - `acc-sem` : Semaphore, for synchronizing updates to accumulator
;;   - `init-acc` : Any, initial accumulator
;;   - `collectors` : [Listof Collector], dependent collectors
;;   - `blame-box` : [Box Blame], merges blames from every value
;;   - `val-hash` : Value-Hash, from collectors to currently stored values
;;   - `folder` : Procedure, calculates the next accumulator
(struct checker (acc-box acc-sem init-acc collectors blame-box val-hash folder))

;; `Value-Hash` ≜ [Or #f [Mutable-Hash Symbol Any]]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; checker

;; Procedure → Procedure
;; If the folding procedure doesn't accept keyword arguments, wrap it with
;; those keywords and ignore their values.
(define (checker-wrap-folder folder)
  (define-values (kws _)
    (procedure-keywords folder))
  (if (empty? kws)
      (λ (#:blame blm . rst)
        (apply folder rst))
      folder))

;; Checker Collector Blame Any Party → Void
;; The fast path is where there is only one variable dependency for the
;; checker, so the `val-hash` is not needed. The slow path has the requires
;; the `val-hash` for "trigger" behavior. Each updates the accumulator
;; and raises an error if there is a violation.
(define (checker-update! ch col blm val neg)
  (define blm* (blame-add-missing-party blm neg))
  (if (checker-val-hash ch)
      (checker-update-slow! ch col blm* val)
      (checker-update-fast! ch col blm* val)))

;; Checker Collector Blame Any → Void
(define (checker-update-fast! ch col blm val)
  (match-define (struct** checker (acc-box acc-sem init-acc folder)) ch)
  (call-with-semaphore
   acc-sem
   (λ ()
     (define old-acc (unbox acc-box))
     (define new-acc (folder #:blame blm old-acc val))
     (checker-update-or-fail! ch old-acc new-acc blm val))))

;; Checker Collector Blame Any → Void
(define (checker-update-slow! ch col blm val)
  (match-define (struct** checker (acc-box collectors blame-box val-hash folder)) ch)
  (define blm* (blame-merge (unbox blame-box) blm))
  (set-box! blame-box blm*)
  (hash-set! val-hash col val)
  (when (= (hash-count val-hash) (length collectors))
    (define vals
      (for/list ([col (in-list collectors)])
        (hash-ref val-hash col)))
    (define old-acc (unbox acc-box))
    (define new-acc (apply folder #:blame blm* (cons old-acc vals)))
    (checker-update-or-fail! ch old-acc new-acc blm* vals)
    (set-box! blame-box #f)
    (hash-clear! val-hash)))

;; Checker Any Any Blame Any → Void
(define (checker-update-or-fail! ch old-acc new-acc blm val)
  (match-define (struct** checker (acc-box init-acc blame-box val-hash)) ch)
  (when logger-enable?
    (log-trace-contract-info "check"))
  (match new-acc
    [(fail reset explain)
     (set-box! acc-box (if (eq? reset NO-RESET) init-acc reset))
     (if explain
         (explain)
         (checker-default-error blm val old-acc val-hash))]
    [_ (set-box! acc-box new-acc)]))

;; [Or #f Blame] Blame → Blame
(define (blame-merge b0 b1)
  (cond
    [(not b0) b1]
    [else (blame-update b0 (blame-positive b1) (blame-negative b1))]))

;; Blame Any Box Boolean → Void
(define (checker-default-error blm val old-acc multi?)
  (define msg (list 'given: "~e" (string-join ERROR-MSG-LINES "")))
  (define val* (if multi? (multiple-values val) val))
  (define args (list val* old-acc))
  (apply raise-blame-error blm val* msg args))

;; A struct purely for better error messages.
(struct multiple-values (data)
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (lambda (self) 'values)
      (lambda (self) (multiple-values-data self))))])
