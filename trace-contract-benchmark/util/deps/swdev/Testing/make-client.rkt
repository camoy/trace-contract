#lang racket

(require (only-in json jsexpr?))

(define port/c (and/c natural-number/c (</c 60000) (>/c 10000)))
(define tries/c natural-number/c)

(provide
 port/c

 #; {IP-Address PortNumber [Natural] [-> Void] -> (values [JSexpr -> JSexpr] Custodian)}
 #; (connect-to-server ip p [n] [#:init init])
 ;; attempts to connect to IP address ip at port p n times;
 ;; returns a CALL-SERVICE that conducts "remote calls" and a custodian for the connection
 ;; failure: exn:fail:network 
 connect-to-server-as-caller
 ;; for backwards compatibility 
 (rename-out [connect-to-server-as-caller connect-to-server])


 #; {IP-Address PortNumber [Natural] -> (values [[JSexpr -> JSexpr] -> JSexpr] Custodian)}
 ;; attempts to connect to IP address ip at port p n times;
 ;; returns a receive-service that turns some given function into the receiver for remote calls
 ;; when connected, it runs (init ip)
 ;; failure: exn:fail:network
 (contract-out
  (broken (-> string? any))
  [broken? (-> any/c boolean?)]
  [connect-to-server-as-receiver
   (->* (string? port/c) (tries/c #:init (-> output-port? any))
        (values (-> (-> (or/c eof-object? jsexpr?) (or/c jsexpr? broken?)) any)
                custodian?))]))

;; ---------------------------------------------------------------------------------------------------
(require "communication.rkt")

;; ---------------------------------------------------------------------------------------------------
(define TCP-TRIES 10)

(define (connect-to-server-as-caller server port (tries TCP-TRIES))
  (define-values (custodian in out) (connect server port tries))
  #; {JSexpr -> JSexpr}
  (define (call-server j)
    (send-message j out)
    (read-message in))
  (values call-server custodian))

(struct broken [x] #:transparent)
#; {type Broken = (broken String)}
;; allow connect-to-server-as-receiver to send alternative strings 

(define (connect-to-server-as-receiver server port (tries TCP-TRIES) #:init (init void))
  (define-values (custodian in out) (connect server port tries))
  (init out)
  #; {[JSexpr -> JSexpr] -> Void}
  (define (receive-from-server f)
    (define input (read-message in))
    (match (f input)
      [(broken result) (displayln result out) (flush-output out)]
      [(? jsexpr? result) (send-message result out)]
      [else (log-error "not (broken) JSON, shutting down") (custodian-shutdown-all custodian)]))
  (values receive-from-server custodian))

#; {IP-Address Port N -> (values Custodian Input-Port Output-Port)}
(define (connect server port tries)
  (define custodian (make-custodian))
  (define-values (in out)
    (parameterize ((current-custodian custodian))
      (let tcp ([n TCP-TRIES])
        (with-handlers ([exn:fail:network? (λ (xn) (sleep 1) (if (<= n 0) (raise xn) (tcp (- n 1))))])
          (tcp-connect server port)))))
  (values custodian in out))
