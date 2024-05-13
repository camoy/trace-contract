#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide suspect<%>
         setof-suspect
         listof-suspect
         listof-witness
         track)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (for-syntax racket/base
                     syntax/parse)
         racket/class
         racket/contract
         racket/match
         racket/list
         racket/set
         racket/string
         "checker.rkt"
         "fail.rkt"
         "trace-contract-macro.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; constants

(define ERR-MSG-LINE "\n  accumulator: ~a")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; interface

(define suspect<%>
  (interface ()
    add
    message))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; implementations

(define setof-suspect
  (new
   (class* object% (suspect<%>)
     (super-new)
     (init-field data)
     (define/public (add x _) (new this% [data (set-add data x)]))
     (define/public (message)
       (format "  suspects: ~a" (set->list data))))
   [data (set)]))

(define listof-suspect
  (new
   (class* object% (suspect<%>)
     (super-new)
     (init-field data)
     (define/public (add x _)
       (new this% [data (cons x data)]))
     (define/public (message)
       (format "  suspects: ~a" (reverse data))))
   [data null]))

(define listof-witness
  (new
   (class* object% (suspect<%>)
     (super-new)
     (init-field data)
     (define/public (add _ x)
       (new this% [data (if (singleton? x)
                            (append x data)
                            (cons x data))]))
     (define/public (message)
       (format "  witnesses: ~v" (reverse data))))
   [data null]))

(define (singleton? x)
  (empty? (rest x)))

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
    (define sus* (send sus add (blame-positive blm) args))
    (match (apply folder* #:blame blm acc args)
      [(fail reset explain)
       (define reset* (cons (if (eq? reset NO-RESET) init-acc reset) init-sus))
       (define explain* (or explain (make-explain blm args acc sus*)))
       (fail reset* explain*)]
      [res (cons res sus*)])))

;; Blame Any Box Suspect → Procedure
(define (make-explain blm val old-acc sus)
  (define sus-msg (send sus message))
  (define msg
    (list 'given: "~e"
          (string-join (list ERR-MSG-LINE sus-msg) "\n")))
  (λ () (raise-blame-error blm val msg val old-acc)))
