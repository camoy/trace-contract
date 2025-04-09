#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide object-trace/c
         this-contract)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (for-syntax racket/base
                     racket/syntax
                     syntax/parse)
         "fail.rkt"
         "trace-contract-macro.rkt"
         automata/machine
         racket/class
         racket/contract
         racket/function
         racket/stxparam)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; syntax

(define-syntax-parameter this-contract
  (λ (stx)
    (raise-syntax-error #f "use of a this-contract not in object-trace/c" stx)))

(begin-for-syntax
  (struct predicate-ast (pos stx))
  (struct include-ast (stx))
  (struct extend-ast (stx))

  (define-syntax-class method
    #:attributes (name [arg-name 1] [arg-ctc 1] res-name res-ctc make)
    #:literals (->m ->dm)
    (pattern [name:id (->m arg-ctc:expr ... res-ctc)
                      (~optional make:expr #:defaults ([make #'NONE]))]
             #:attr (arg-name 1) (map (λ _ #f) (syntax-e #'(arg-ctc ...)))
             #:attr res-name #f)
    (pattern [name:id (->dm ([arg-name:id arg-ctc:expr] ...)
                            [res-name:id res-ctc:expr])
                      (~optional make:expr #:defaults ([make #'NONE]))]))

  (define-splicing-syntax-class option
    #:attributes (ast)
    (pattern (~seq #:satisfies mach:expr)
             #:attr ast (predicate-ast #t (syntax-local-lift-expression #'mach)))
    (pattern (~seq #:refutes mach:expr)
             #:attr ast (predicate-ast #f (syntax-local-lift-expression #'mach)))
    (pattern (~seq #:include e:expr)
             #:attr ast (include-ast #'e))
    (pattern (~seq #:extend e:expr)
             #:attr ast (extend-ast #'e))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; object-trace

(define NONE (gensym))

(define-syntax object-trace/c
  (syntax-parser
    #:datum-literals (property)
    [(_ ?o:option ... m:method ...)
     #:with t (generate-temporary #'_)
     #:with (include-id ...)
     (for/list ([ast (in-list (attribute ?o.ast))]
                #:when (include-ast? ast))
       (include-ast-stx ast))
     #:with (extend-id ...)
     (for/list ([ast (in-list (attribute ?o.ast))]
                #:when (extend-ast? ast))
       (extend-ast-stx ast))
     #:with (predicate-clause ...)
     (for/list ([ast (in-list (attribute ?o.ast))]
                #:when (predicate-ast? ast))
       (define prop (predicate-ast-stx ast))
       (define pos (predicate-ast-pos ast))
       (define folder (syntax-local-lift-expression #`(make-folder #,prop #,pos)))
       #`(accumulate #,prop [(t) #,folder] [(include-id) #,folder] ...))
     #'(trace/c ([t any/c])
         (object/c
           (~? [m.name
                (syntax-parameterize ([this-contract (make-rename-transformer #'t)])
                  (->i ([this any/c] [m.arg-name m.arg-ctc] ...)
                       [_ (this m.arg-name ...) (push/c m.res-ctc (λ (m.res-name) m.make) t extend-id ...)]))]
               [m.name
                (syntax-parameterize ([this-contract (make-rename-transformer #'t)])
                  (-> (push/c any/c (λ (this) m.make) t extend-id ...) m.arg-ctc ... m.res-ctc))]) ...)
         predicate-clause ...)]))

(define (make-folder init pos?)
  (define accept?
    (if pos?
        machine-accepting?
        (negate machine-accepting?)))
  (λ (mach evt)
    (define mach* (mach evt))
    (if (accept? mach*)
        mach*
        (make-fail #:reset init))))

(define (push/c res-ctc make . tr-ctcs)
  (define tr-lnps
    (map get/build-late-neg-projection tr-ctcs))
  (define lnp
    (get/build-late-neg-projection
     (coerce-contract 'push/c res-ctc)))
  (make-contract
   #:late-neg-projection
   (λ (blm)
     (define tr-lnp+blms
       (for/list ([tr-lnp (in-list tr-lnps)])
         (tr-lnp blm)))
     (define lnp+blm (lnp blm))
     (λ (val neg)
       (define ret (lnp+blm val neg))
       (for ([tr-lnp+blm (in-list tr-lnp+blms)])
         (define val (make ret))
         (unless (eq? val NONE)
           (tr-lnp+blm val neg)))
       ret))))
