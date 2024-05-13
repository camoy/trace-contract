#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide (for-syntax trace-clause-macro
                     trace-clause-macro?
                     trace-clause-literals
                     trace-clause-expand)
         trace/c
         accumulate
         combine)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (for-syntax ee-lib
                     racket/base
                     racket/list
                     racket/sequence
                     syntax/id-set
                     syntax/parse
                     syntax/stx)
         ee-lib/define
         racket/contract
         racket/function
         racket/list
         racket/set
         "decl.rkt"
         "clause.rkt"
         "collector-contract.rkt"
         "subclause.rkt"
         "trace-contract.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `decls` syntax class

(begin-for-syntax
  (define-syntax-class decls
    #:description "non-empty sequence of unique variable declarations"
    #:attributes (vars norm)
    (pattern (decl:decl ...+)
             #:with vars:ids #'(decl.var ...)
             #:with norm #'(list decl.norm ...)))

  (define-syntax-class decl
    #:description "variable declaration"
    #:attributes (var norm)
    (pattern [var:id ctc]
             #:declare ctc (expr/c #'contract? #:name "variable contract")
             #:with norm #'(decl 'var ctc.c)))

  (define-syntax-class ids
    #:description "non-empty sequence of unique identifiers"
    #:attributes (num)
    (pattern (id:id ...+)
             #:do [(define ids (syntax->list #'(id ...)))]
             #:fail-when
             (check-duplicate-identifier ids)
             "duplicate identifier"
             #:attr num (length ids)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `inners` syntax class

(begin-for-syntax
  (define-syntax-class inners
    #:description "inner contract"
    #:attributes ([norm 1])
    #:literals (values)
    (pattern (values ctc ...+)
             #:declare ctc (expr/c #'contract? #:name "inner contract")
             #:with (norm ...) #'(ctc.c ...))
    (pattern ctc
             #:declare ctc (expr/c #'contract? #:name "inner contract")
             #:with (norm ...) #'(ctc.c)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `clauses` syntax class

(define-literal-forms trace-clause-literals
  "trace clause must occur within trace/c"
  (accumulate combine))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; expander

(begin-for-syntax
  (struct trace-variable ())
  (struct trace-clause-macro (transformer))

  (define/hygienic (trace-clause-expand stx) #:expression
    (syntax-parse stx
      #:literal-sets (trace-clause-literals)
      [(accumulate init-acc:expr [(var:id ...+) folder:expr] ...+)
       (maybe-raise-duplicate-identifier-error #'(var ... ...))
       #;(maybe-raise-missing-identifier-error #'(var ... ...))
       stx]
      [(combine raw-rst ...)
       #:with (rst ...) (stx-map trace-clause-expand #'(raw-rst ...))
       #'(combine rst ...)]
      [(macro-name:id rst ...)
       #:do [(define macro (lookup #'macro-name trace-clause-macro?))]
       #:when macro
       (define transformer (trace-clause-macro-transformer macro))
       (trace-clause-expand (transformer stx))]
      [_ (raise-syntax-error #f "invalid trace clause" stx)]))

  (define (maybe-raise-duplicate-identifier-error ids)
    (define dup-id (check-duplicate-identifier (syntax->list ids)))
    (when dup-id
      (raise-syntax-error #f "duplicate identifier" dup-id)))

  (define (maybe-raise-missing-identifier-error ids)
    (define missing-id
      (for/or ([var (in-syntax ids)])
        (and (not (lookup var trace-variable?)) var)))
    (when missing-id
      (raise-syntax-error #f "trace variable not declared" missing-id)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; compiler

(begin-for-syntax
  (define (trace-clause-compile stx)
    (syntax-parse stx
      #:literal-sets (trace-clause-literals)
      [(accumulate init-acc [(var ...) folder] ...)
       #:declare folder
       (expr/c (let* ([num (length (syntax->list #'(var ...)))])
                 #`(folder/c #,num)))
       (list #'(make-clause init-acc (subclause (list var ...) folder.c) ...))]
      [(combine e ...)
       (append-map trace-clause-compile (syntax->list #'(e ...)))]))
  )

;; Natural → (Procedure → Boolean)
(define (folder/c arity)
  (flat-named-contract
   `(procedure-arity-includes/c ,arity)
   (λ (proc)
     (define-values (kws _) (procedure-keywords proc))
     (and (arity-includes? (procedure-arity proc) (add1 arity))
          (or (empty? kws) (member '#:blame kws))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `trace/c` interface macro

(define-syntax (trace/c stx)
  (with-scope sc
    (syntax-parse stx
      [(_ decls:decls
          (~optional (~and #:global (~bind [?global #'#t]))
                     #:defaults ([?global #'#f]))
          inners:inners
          (~describe "trace clause" raw-clause) ...)
       #:with (decl-var ...) #'decls.vars
       #|
       #:do [(for ([var (in-syntax #'(decl-var ...))])
       (bind! (add-scope var sc) (trace-variable)))]
       |#
       #:with (body ...)
       (for/list ([body (in-syntax #'(inners.norm ...))])
         body #;(add-scope body sc))
       #:with (clause ...)
       (trace-clause-compile
        (trace-clause-expand
         #'(combine raw-clause ...)
         #;(add-scope #'(combine raw-clause ...) sc)))
       #`(make-trace-contract
          '#,stx
          decls.norm
          ?global
          (list (λ decls.vars
                  (clause-register! clause) ...
                  body) ...)
          (current-contract-region))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests

(module+ test
  (require chk
           syntax/macro-testing)

  (chk
   #:x
   (convert-syntax-error
    (trace/c ([x integer?] [x integer?])
      any/c
      (accumulate 0 [(x) values])))
   "duplicate identifier"

   #:x
   (convert-syntax-error
    (trace/c ([x integer?])
      any/c
      (accumulate 0 [(x x) values])))
   "duplicate identifier"

   #:x
   (convert-syntax-error
    (trace/c ([x integer?])
      any/c
      (accumulate 0 [(y) values])))
   "trace variable not declared"

   #:x
   (convert-syntax-error
    (trace/c ([x integer?])
      any/c))
   "expected more terms starting with trace clause"

   #:x
   (convert-syntax-error
    (trace/c ([x integer?])
      any/c
      (accumulate)))
   "invalid trace clause"

   #:x
   (convert-syntax-error
    (trace/c ([x integer?])
      any/c
      (accumulate 0)))
   "invalid trace clause"

   #:x
   (convert-syntax-error
    (trace/c ([x integer?])
      any/c
      (accumulate 0 [() values])))
   "invalid trace clause"

   #:x
   (convert-syntax-error
    (accumulate 0))
   "trace clause must occur within trace/c"

   #:x
   (contract
    (trace/c ([x integer?])
      #'hello
      (accumulate 0 [(x) values]))
    0 'pos 'neg)
   "macro argument contract on inner contract"

   #:x
   (contract
    (trace/c ([x #'hello])
      any/c
      (accumulate 0 [(x) values]))
    0 'pos 'neg)
   "macro argument contract on variable contract"

   #:x
   (contract
    (trace/c ([x integer?])
      any/c
      (accumulate 0 [(x) (λ () 42)]))
    0 'pos 'neg)
   "contract violation"
   ))
