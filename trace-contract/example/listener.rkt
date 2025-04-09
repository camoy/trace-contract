#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (except-in "util/test.rkt"
                    match
                    define-match-expander)
         contract-etc
         logic/re
         logic/qea
         logic/match)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define port-re
  (re (seq (star `use)
           (opt (union `close-stream
                       `(close-listener ,_))))))

(define (input-stream/c listener/c)
  (object-trace/c
   #:satisfies port-re
   #:include listener/c
   [read-byte (->m byte?) `use]
   [peek-byte (->m byte?) `use]
   [close (->m void?)     `close-stream]))

(define (output-stream/c listener/c)
  (object-trace/c
   #:satisfies port-re
   #:include listener/c
   [write-byte (->m byte? void?) `use]
   [close (->m void?) `close-stream]))

(define (tcp-listener/c manager/c port)
  (object-trace/c
   #:extend manager/c
   [close (->m void?) `(close-listener ,port)]
   [accept (->m (values (input-stream/c this-contract)
                        (output-stream/c this-contract)))]))

(define manager-qea
  (qea
   (∀ port)
   (start ready)
   [-> ready `(listen ,port) listening]
   [-> listening `(close-listener ,port) ready]))

(define tcp-manager/c
  (object-trace/c
   #:satisfies manager-qea
   [listen (->dm ([port listen-port-number?])
                 [res (tcp-listener/c this-contract port)])
           `(listen ,port)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; stubs

(define/contract tcp-manager%
  (classof/c tcp-manager/c)
  (class object%
    (super-new)
    (define/public (listen port)
      (new tcp-listener%))))

(define tcp-listener%
  (class object%
    (super-new)
    (define/public (accept)
      (values (new input-stream%) (new output-stream%)))
    (define/public (close)
      (void))))

(define input-stream%
  (class object%
    (super-new)
    (define/public (read-byte) 65)
    (define/public (peek-byte) 65)
    (define/public (close) (void))))

(define output-stream%
  (class object%
    (super-new)
    (define/public (write-byte b) (void))
    (define/public (close) (void))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; tests

(module+ test
  (require chk)

  (let* ([manager (new tcp-manager%)]
         [listener (send manager listen 8080)])
    (chk
     #:x (send manager listen 8080)
     "produced: '(listen 8080)"))

  (let* ([manager (new tcp-manager%)]
         [listener (send manager listen 8080)])
    (chk
     #:t (send manager listen 8000)
     #:t (send listener close)
     #:t (send manager listen 8080)
     #:x (send manager listen 8000)
     "produced: '(listen 8000)"
     ))

  (let* ([manager (new tcp-manager%)]
         [listener (send manager listen 8080)])
    (define-values (in out)
      (send listener accept))
    (chk
     #:t (send in read-byte)
     #:t (send in close)
     #:x (send in read-byte)
     "given: 'use"
     ))

  (let* ([manager (new tcp-manager%)]
         [listener (send manager listen 8080)])
    (define-values (in out)
      (send listener accept))
    (chk
     #:t (send out write-byte 10)
     #:t (send listener close)
     #:x (send out write-byte 10)
     "given: 'use"
     )))
