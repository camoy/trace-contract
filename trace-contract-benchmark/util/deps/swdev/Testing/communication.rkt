#lang racket 

;; ---------------------------------------------------------------------------------------------------
(provide
 ;; send parameters 
 trailing-newline?    ;; by default, #t
 pretty-print-output? ;; all others are off by default 
 trickle-output?      ;; boolean? 
 encode-all-unicode?  ;; boolean?
 prefix-with-spaces   ;; natural? 

 #; {JSExpr [OutputPort] -> (U False  Void)}
 ;; effect: writes message to current output port, adds newline and flushes
 send-message

 #; {[InputPort] -> (U JSexpr x : terminal-value?)}
 ;; Read a blob of JSON, treating any network error as EOF, and only waiting for TIMEOUT seconds.
 ;; (Because tcp-read gets RST from linux servers from time to time.)
 read-message

 #; {Any -> Boolean : (U  NO-REACTION RESPONSE-INCOMPLETE (list ERROR String))}
 ;; what kind of error was triggered
 terminal-value?
 
 NO-REACTION
 RESPONSE-INCOMPLETE

 TIMEOUT ;; seconds
 ;; (client gets this many sec. to start sending JSON, and this many more to complete the sending)

 ;; the number of s that the test harness is going to wait for a message from the tested program 
 io-time-out

 ;; for backwards compatibility 
 #; {#:limit -> Void}
 ;; sets `io-time-out` 
 unset-time-out)

;; ---------------------------------------------------------------------------------------------------
(require "../Lib/json-pretty.rkt")
(require json)
(require racket/exn)

;                              
;                            ; 
;                            ; 
;                            ; 
;    ;;;    ;;;   ; ;;    ;;;; 
;   ;   ;  ;;  ;  ;;  ;  ;; ;; 
;   ;      ;   ;; ;   ;  ;   ; 
;    ;;;   ;;;;;; ;   ;  ;   ; 
;       ;  ;      ;   ;  ;   ; 
;   ;   ;  ;      ;   ;  ;; ;; 
;    ;;;    ;;;;  ;   ;   ;;;; 
;                              
;                              
;                              

(define trailing-newline?    (make-parameter #t))
(define pretty-print-output? (make-parameter #f))
(define trickle-output?      (make-parameter #f))
(define encode-all-unicode?  (make-parameter #f))
(define prefix-with-spaces   (make-parameter 0))

(define (send-message i (oport (current-output-port)))
  (parameterize ((current-output-port oport))
    (with-handlers ([exn:fail:network? (lambda (e) #f)]
                    [exn:fail:filesystem? (lambda (e) #f)])
      (define output-bytes (format-as-bytes i))
      (send-bytes-to-port output-bytes)
      (if (trailing-newline?) (newline) (write-byte 32))
      (flush-output))))

#; {JSExpr -> Bytes}
(define (format-as-bytes i)
  (if (pretty-print-output?)
      (with-output-to-bytes (λ () (write-json/pretty i #:indent-maps? #t #:indent-lists? #t)))
      (jsexpr->bytes i #:encode (if (encode-all-unicode?) 'all 'control))))

#; {Bytes -> Void}
(define (send-bytes-to-port output-bytes)
  (define output-length (bytes-length output-bytes))
  (define chunk-size    (max (quotient output-length 100) 10))
  (write-bytes (make-bytes (prefix-with-spaces) (bytes-ref #" " 0)))
  (if (not (trickle-output?))
      (write-bytes output-bytes)
      (for [(offset (in-range 0 output-length chunk-size))]
        (define chunk (subbytes output-bytes offset (min output-length (+ offset chunk-size))))
        (write-bytes chunk)
        (flush-output)
        (sleep 0.005))))

;                              
;                            ; 
;                            ; 
;                            ; 
;    ;;;;   ;;;   ;;;;    ;;;; 
;    ;;  ; ;;  ;      ;  ;; ;; 
;    ;     ;   ;;     ;  ;   ; 
;    ;     ;;;;;;  ;;;;  ;   ; 
;    ;     ;      ;   ;  ;   ; 
;    ;     ;      ;   ;  ;; ;; 
;    ;      ;;;;   ;;;;   ;;;; 
;                              
;                              
;                              
(define NO-REACTION         "Timed out waiting for reading to start.")
(define RESPONSE-INCOMPLETE "Timed out waiting for reading to complete.")

(define TIMEOUT 10) ;; seconds. See read-json/timeout.
(define io-time-out (make-parameter TIMEOUT))

(define (unset-time-out #:limit (limit 1000))
  (io-time-out limit))

(define (read-message (iport (current-input-port)))
  (parameterize ((current-input-port iport))
    (with-handlers ([exn:fail:network? (lambda (_exn) (log-error "read-message: fail:network") eof)])
      (read-json/timeout (io-time-out) (io-time-out)))))

;; Detects values that mean the end of the session with the remote party.
(define (terminal-value? blob)
  (or (eq? blob  NO-REACTION)
      (eq? blob  RESPONSE-INCOMPLETE)
      (and (string? blob) (regexp-match #px"ERROR" blob))))

;; Read a blob of JSON with a timeout for the first byte of input to appear
;; and a second timeout by which the entirety of the blob should have appeared.
(define (read-json/timeout start-timeout-sec response-duration-timeout-sec)
  (define control-ch  (make-channel))
  (define reply-ch    (make-channel))
  (define read-thread (thread (make-reader control-ch reply-ch start-timeout-sec)))
  (retrieve-response control-ch reply-ch response-duration-timeout-sec))

#; {-> [Channel Channel N -> Void]}
;; EFFECT when the input port is ready, try to read-json & send result on reply-ch
;; EFFECT tell control thread on control-ch that the reading has (not) started in `start-timeout`
(define ((make-reader control-ch reply-ch start-timeout-sec))
  (cond
    [(sync/timeout start-timeout-sec (current-input-port))
     (channel-put control-ch 'response-started)
     (with-handlers [(values (lambda (e) (channel-put reply-ch (list 'exn e))))]
       (channel-put reply-ch (list 'ok (read-json))))]
    [else
     (channel-put control-ch 'response-not-started)]))

#; {-> [Channel Channel N -> (U NO-REACTION ERROR-string RESPONSE-INCOMPLETE JSexpr)]}
;; retrieve the JSON value after it was read subject to timing constraints 
;; EFFECT wait for a signal on control channel; 
;; EFFECT if the signal is 'response-started, enforce time-out for the reading process
(define (retrieve-response control-ch reply-ch response-duration-timeout-sec)
  (match (channel-get control-ch)
    ['response-not-started NO-REACTION]
    ['response-started
     (match (sync/timeout response-duration-timeout-sec reply-ch)
       [(list 'ok blob)                   blob]
       [(list 'exn (? exn:fail:network?)) eof]
       [(list 'exn e)                     (string-append "ERROR" " " (exn-message e))]
       [#f                                RESPONSE-INCOMPLETE])]))

