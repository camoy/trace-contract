#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide make-checker-hash
         checker-wrap-folder
         checker-update!
         checker-default-error)

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
;;   - `init-acc` : Any, initial accumulator
;;   - `vars` : [Listof Symbol], dependent variables
;;   - `blame-box` : [Box Blame], merges blames from every value
;;   - `val-hash` : Value-Hash, from variables to currently stored values
;;   - `folder` : Procedure, calculates the next accumulator
(struct checker (acc-box init-acc vars blame-box val-hash folder))

;; `Value-Hash` ≜ [Or #f [Mutable-Hash Symbol Any]]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; checker hash

;; [Listof Clause] → Checker-Hash
;; Constructs a checker hash with fresh states for the given clauses.
(define (make-checker-hash cls)
  (for/fold ([ht (hash)])
            ([cl (in-list cls)])
    (hash-union ht (make-checker-hash* cl) #:combine set-union)))

;; Clause → Checker-Hash
(define (make-checker-hash* cl)
  (match-define (struct** clause (init-acc subclauses)) cl)
  (define acc-box (box init-acc))
  (for/fold ([ht (hash)])
            ([subcl (in-list subclauses)])
    (match-define (struct** subclause (vars folder)) subcl)
    (define blame-box (box #f))
    (define val-hash (and (> (length vars) 1) (make-hash)))
    (define folder* (checker-wrap-folder folder))
    (define ch (checker acc-box init-acc vars blame-box val-hash folder*))
    (hash-add-set ht vars ch)))

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

;; Checker Symbol Blame Any Party → Void
;; The fast path is where there is only one variable dependency for the
;; checker, so the `val-hash` is not needed. The slow path has the requires
;; the `val-hash` for "trigger" behavior. Each updates the accumulator
;; and raises an error if there is a violation.
(define (checker-update! ch var blm val neg)
  (define blm* (blame-add-missing-party blm neg))
  (if (checker-val-hash ch)
      (checker-update-slow! ch var blm* val)
      (checker-update-fast! ch var blm* val)))

;; Checker Symbol Blame Any → Void
(define (checker-update-fast! ch var blm val)
  (match-define (struct** checker (acc-box init-acc folder)) ch)
  (define old-acc (unbox acc-box))
  (define new-acc (folder #:blame blm old-acc val))
  (checker-update-or-fail! ch old-acc new-acc blm val))

;; Checker Symbol Blame Any → Void
(define (checker-update-slow! ch var blm val)
  (match-define (struct** checker (acc-box vars blame-box val-hash folder)) ch)
  (define blm* (blame-merge (unbox blame-box) blm))
  (set-box! blame-box blm*)
  (hash-set! val-hash var val)
  (when (= (hash-count val-hash) (length vars))
    (define vals
      (for/list ([var (in-list vars)])
        (hash-ref val-hash var)))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests

(module+ test
  (require chk
           racket/contract/private/blame
           racket/stream
           (submod "clause.rkt" example))

  (define (make-blame* pos)
    (make-blame (srcloc #f #f #f #f #f) #f (λ () #f) pos #f #t))
  (define blm (make-blame* 'pos))

  (chk
   #:do (match-define (hash-table ('x (stream pos-ch)))
          (make-checker-hash (list positive-clause)))
   #:do (match-define (hash-table ('x (stream even-ch)) ('y (stream ven-ch)))
          (make-checker-hash (list even-clause)))

   #:do (checker-update! pos-ch 'x blm 10 'neg)
   #:x (checker-update! pos-ch 'x blm -20 'neg)
   "accumulator: 11"

   #:do (checker-update! even-ch 'x blm 10 'neg)
   #:do (checker-update! even-ch 'y blm 6 'neg)
   #:do (checker-update! even-ch 'x blm 10 'neg)
   #:x (checker-update! even-ch 'y (blame-swap blm) 3 'neg)
   "accumulator: 16"
   ))
