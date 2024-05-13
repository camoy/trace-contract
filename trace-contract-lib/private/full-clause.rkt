#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide full
         trace?
         trace-merge)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (for-syntax racket/base
                     syntax/parse)
         racket/contract
         racket/match
         racket/list
         racket/stream
         racket/string
         "fail.rkt"
         "trace-contract-macro.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; constants

(define ERROR-MSG-LINES
  '("\n  traces ~a"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; data

(struct trace (vals)
  #:methods gen:stream
  [(define (stream-empty? st)
     (match-define (trace vals) st)
     (empty? vals))

   (define (stream-first st)
     (match-define (trace vals) st)
     (car (first vals)))

   (define (stream-rest st)
     (match-define (trace vals) st)
     (trace (rest vals)))])

(define (trace-merge . trs)
  (define vals
    (sort (append-map trace-vals trs)
          (λ (x y) (< (cdr x) (cdr y)))))
  (trace vals))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; macro

(define-syntax full
  (trace-clause-macro
   (syntax-parser
     [(_ (?id:id ...+) ?e:expr)
      #:with (?k ...)
      (for/list ([k (in-range (length (syntax->list #'(?id ...))))])
        #`#,k)
      #:with ?folder
      (syntax-local-lift-expression
       #'(full-folder '(?id ...) ?e))
      #'(accumulate
         (full-init-acc '(?id ...))
         [(?id) (?folder ?k)] ...)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; folder

(define (full-init-acc ids)
  (for/list ([id (in-list ids)])
    (trace null)))

(define (((full-folder ids f) acc-k) #:blame blm accs val)
  (define timestamp
    (for/sum ([tr (in-list accs)])
      (length (trace-vals tr))))
  (define accs*
    (for/list ([k (in-naturals)]
               [tr (in-list accs)])
      (cond
        [(= acc-k k)
         (define new-val (cons val timestamp))
         (trace (append (trace-vals tr) (list new-val)))]
        [else tr])))
  (if (not (apply f accs*))
      (make-fail #:explain (full-explainer ids accs* blm val))
      accs*))

(define (full-explainer ids accs blm val)
  (λ ()
    (define acc-strings
      (for/list ([id (in-list ids)]
                 [acc (in-list accs)])
        (format "\n    ~a: ~a" id (map car (trace-vals acc)))))
    (define acc-string (string-join acc-strings ""))
    (define msg (list 'given: "~e" (string-join ERROR-MSG-LINES "")))
    (define args (list val acc-string))
    (apply raise-blame-error blm val msg args)))
