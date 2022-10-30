#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide suspect<%>
         setof-suspect
         listof-suspect
         track)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (for-syntax racket/base
                     syntax/parse)
         racket/class
         racket/contract
         racket/match
         racket/set
         racket/string
         "checker.rkt"
         "fail.rkt"
         "trace-contract-macro.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; constants

(define ERROR-MSG-LINES
  '("\n  accumulator: ~a"
    "\n  suspects: ~a"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; interface

(define suspect<%>
  (interface ()
    add
    value))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; implementations

(define setof-suspect
  (new
   (class* object% (suspect<%>)
     (super-new)
     (init-field data)
     (define/public (add x) (new this% [data (set-add data x)]))
     (define/public (value) (set->list data)))
   [data (set)]))

(define listof-suspect
  (new
   (class* object% (suspect<%>)
     (super-new)
     (init-field data)
     (define/public (add x) (new this% [data (cons x data)]))
     (define/public (value) (reverse data)))
   [data null]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; macro

(define-syntax track
  (trace-clause-macro
   (syntax-parser
     [(_ ?sus ?cl ...+)
      #:declare ?sus (expr/c #'(is-a?/c suspect<%>))
      #:with ?sus-id (syntax-local-lift-expression #'?sus.c)
      #:with ?cls (trace-clause-expand #'(combine ?cl ...))
      #'(track* ?sus-id ?cls)])))

(define-syntax track*
  (trace-clause-macro
   (syntax-parser
     #:literal-sets (trace-clause-literals)
     [(_ ?sus-id (accumulate ?init:expr [(id:id ...+) folder:expr] ...))
      #'(accumulate (cons ?init ?sus-id)
          [(id ...) (make-folder folder ?init ?sus-id)] ...)]
     [(_ ?sus-id (combine ?cl ...))
      #'(combine (track* ?sus-id ?cl) ...)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; runtime

;; Procedure Any Suspect → Procedure
(define (make-folder folder init-acc init-sus)
  (define folder* (checker-wrap-folder folder))
  (λ (acc+sus #:blame blm . args)
    (match-define (cons acc sus) acc+sus)
    (define sus* (send sus add (blame-positive blm)))
    (match (apply folder* #:blame blm acc args)
      [(fail reset explain)
       (define reset* (cons (if (eq? reset NO-RESET) init-acc reset) init-sus))
       (define explain* (or explain (make-explain blm args acc sus*)))
       (fail reset* explain*)]
      [res (cons res sus*)])))

;; Blame Any Box Suspect → Procedure
(define (make-explain blm val old-acc sus)
  (define msg (list 'given: "~e" (string-join ERROR-MSG-LINES "")))
  (define args (list val old-acc (send sus value)))
  (λ () (apply raise-blame-error blm val msg args)))
