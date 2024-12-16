#lang racket

;; turn ports into objects to allow for conditional processing of inputs at the `send-message` step
;; do it for both input and output ports for uniformity

;; POTENTIAL ISSUE
;; -- `port-object?` covers both input and output ports 

;; ---------------------------------------------------------------------------------------------------
(provide
 ;; SYNTAX
 #; (channel2 f g)
 ;; runs `g` to get 2 values and applies f
 channel2

 (contract-out
  [ports->objects
   (-> input-port? output-port? (values port-object? port-object?))]
  
  [combine-output-ports
   ;; the port counts the messages it is supposed to `send-message`;
   ;; pass count to the two optional functions 
   (->* (port-object? port-object?) ([-> natural? any/c] [-> natural? any/c]) port-object?)]))

;; ---------------------------------------------------------------------------------------------------
(require "communication.rkt")

(module+ test
  (require (submod ".."))
  (require rackunit))

;; ---------------------------------------------------------------------------------------------------
(define-syntax-rule (channel2 f g)
  (let-values {([a b] g)}
    (f a b)))

(define (port-object? x)
  (or (is-a? x in-port%) (is-a? x out-port%) (is-a? x combined-output-port%)))

(define (ports->objects in out)
  (values (new in-port% [in in]) (new out-port% [out out])))

(define (combine-output-ports out1 out2 [pre1 identity] [pre2 identity])
  (new combined-output-port% [server-out out1] [client-out out2] [for-server pre1] [for-client pre2]))

(define in-port%
  (class object% (init-field in)
    (define/public (read) (read-message in))
    (define/public (close) (close-input-port in))
    (super-new)))

(define out-port%
  (class object% (init-field out)
    (define/public (message x) (send-message x out))
    (define/public (close) (close-output-port out))
    (super-new)))

(define combined-output-port%
  (class object%
    (init-field #;{[Instance OutPort%]} server-out)
    (init-field #;{[Instance OutPort%]} client-out)
    (init-field #;{[Listof JSexpr] -> [Listof JSexpr]} [for-server identity])
    (init-field #;{[Listof JSexpr] -> [Listof JSexpr]} [for-client identity])

    (define *count 0) 

    (define/public (message x)
      (when (for-server *count) (send server-out message x))
      (when (for-client *count) (send client-out message x))
      (set! *count (+ *count 1)))

    (define/public (close)
      (send server-out close)
      (send client-out close))
    
    (super-new)))

;; ---------------------------------------------------------------------------------------------------
(module+ test
  (check-equal?
   (let ([s (open-output-string)])
     (parameterize ([current-output-port s])
       (define-values [_ out] (ports->objects (current-input-port) (current-output-port)))
       (send out message 42))
     (get-output-string s))
   "42\n"
   "ports to objects")

  (check-equal?
   (let ([s (open-output-string)])
     (parameterize ([current-output-port s]
                    [current-error-port s])
       (define out1 (new out-port% [out (current-output-port)]))
       (define out2 (new out-port% [out (current-error-port)]))
       (define out (combine-output-ports out1 out2))
       (send out message 42))
     (get-output-string s))
   "42\n42\n"
   "combined port")
  
  (check-equal?
   (let ([s (open-output-string)])
     (parameterize ([current-output-port s]
                    [current-error-port s])
       (define out1 (new out-port% [out (current-output-port)]))
       (define out2 (new out-port% [out (current-error-port)]))
       (define out (combine-output-ports out1 out2 zero? (λ (i) (= i 1))))
       (send out message 42)
       (send out message 21))
     (get-output-string s))
   "42\n21\n"
   "combined port"))
