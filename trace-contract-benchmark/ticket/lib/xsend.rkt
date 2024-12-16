#lang racket

;; a library that protects calls from exceptions and overly slow clients

;; if a contract on the method must print values, use
#; (error-printing-thread formatter)
;; to spawn a thread and obtain a channel for printing debugging info
;; even if the timed thread gets killed (before that).

;
;
;                                                            ;;;
;                     ;                                        ;
;                     ;                                        ;
;    ;;;;   ;;  ;;  ;;;;;;   ;;;;    ;;;;   ; ;;;     ;;;      ;
;   ;    ;   ;  ;     ;     ;    ;   ;;  ;  ;;   ;   ;   ;     ;
;   ;;;;;;    ;;      ;     ;;;;;;   ;      ;    ;       ;     ;
;   ;         ;;      ;     ;        ;      ;    ;   ;;;;;     ;
;   ;         ;;      ;     ;        ;      ;    ;  ;    ;     ;
;   ;;   ;   ;  ;     ;     ;;   ;   ;      ;    ;  ;   ;;     ;
;    ;;;;;  ;    ;     ;;;   ;;;;;   ;      ;    ;   ;;; ;      ;;;
;
;
;
;

(provide

 show-exn

 #;{Parameter Real}
 (contract-out
  [time-out-limit (parameter/c (λ (d) (and (real? d) (positive? d))))])

 #;{Any -> Boolean}
 failed?
 failed-value

 #;(xsend object method args ...)
 ;; returns a failed value of method raises an exception or exceeds time-out-limit
 xsend

 #;{ ([X ...] -> Y) X ... #:throw-handler (Z -> W) #:time-out (-> V) #:f-msg-format FmtString -> Y }
 xcall

 #; {type Action = [Player Message -> (or/c failed? any/c)]}
 ;; this could either send a message (xsend) or call a function (xcall)

 #; {[Actor -> X] [Listof Actor] -> (values [Listof [List Actor X]] [Listof Actor])}
 #; (call-and-report action actors)
 ;; call action on all actors
 ;; -- successful actors get paired with their result, in the order in which they were given
 ;; -- failing actors get returned in the second list, in the order in which they were given
 ;; -- failing actors are never called twice:
 ;; -- ASSUME actors are not necessarily distinct
 xmap-send

 #; {[X -> Void] -> [Channelof X]}
 error-printing-thread)

;
;
;        ;                                       ;                             ;
;        ;                                       ;
;        ;                                       ;
;    ;;; ;   ;;;;   ; ;;;    ;;;;   ; ;;;    ;;; ;   ;;;;   ; ;;;     ;;;    ;;;     ;;;;    ;;;;
;   ;;  ;;  ;    ;  ;;  ;;  ;    ;  ;;   ;  ;;  ;;  ;    ;  ;;   ;   ;   ;     ;    ;    ;  ;    ;
;   ;    ;  ;;;;;;  ;    ;  ;;;;;;  ;    ;  ;    ;  ;;;;;;  ;    ;  ;          ;    ;;;;;;  ;
;   ;    ;  ;       ;    ;  ;       ;    ;  ;    ;  ;       ;    ;  ;          ;    ;        ;;;;
;   ;    ;  ;       ;    ;  ;       ;    ;  ;    ;  ;       ;    ;  ;          ;    ;            ;
;   ;;  ;;  ;;   ;  ;;  ;;  ;;   ;  ;    ;  ;;  ;;  ;;   ;  ;    ;   ;   ;     ;    ;;   ;  ;    ;
;    ;;; ;   ;;;;;  ; ;;;    ;;;;;  ;    ;   ;;; ;   ;;;;;  ;    ;    ;;;    ;;;;;   ;;;;;   ;;;;
;                   ;
;                   ;
;                   ;
;

(require (for-syntax syntax/parse))
#;(module+ test
  (require rackunit))

;
;
;                                                                        ;
;                     ;                                                  ;
;                     ;                                                  ;
;    ;;;;   ;;  ;;  ;;;;;;                   ;;;;    ;;;;   ; ;;;    ;;; ;
;   ;    ;   ;  ;     ;                     ;    ;  ;    ;  ;;   ;  ;;  ;;
;   ;;;;;;    ;;      ;                     ;       ;;;;;;  ;    ;  ;    ;
;   ;         ;;      ;                      ;;;;   ;       ;    ;  ;    ;
;   ;         ;;      ;       ;;                 ;  ;       ;    ;  ;    ;
;   ;;   ;   ;  ;     ;       ;;            ;    ;  ;;   ;  ;    ;  ;;  ;;
;    ;;;;;  ;    ;     ;;;    ;;             ;;;;    ;;;;;  ;    ;   ;;; ;
;
;
;
;

(define EXN:fmt "xdynamic-send: ~a raised an exception for ~a:\n")

(struct failed (value) #:transparent)

(define time-out-limit (make-parameter .6))
(define show-exn (make-parameter #false))

(define-syntax (xsend stx)
  (syntax-parse stx
    [(xsend o m (~optional (~seq #:caller name)) a ...)
     (with-syntax ([n (if (attribute name) #'name #'#false)])
       #'(xdynamic-send o 'm #:caller n a ...))]))

(define (xmap-send action actors)
  (let loop ([actors actors] [failures '()] [actor+result* '()])
    (match actors
      ['() (values (reverse actor+result*) (reverse failures))]
      [(cons next-a others)
       (define result (action next-a))
       (if (failed? result)
           (loop (remove* (list next-a) others) (cons next-a failures) actor+result*)
           (loop others failures (cons (list next-a result) actor+result*)))])))

(define (xdynamic-send target m
                       #:caller (name #false)
                       #:thrown [throw-hdler failed]
                       #:timed-out [time-out-hdler (λ _ (failed 'time))]
                       . a)
  (define fmt (string-append (format EXN:fmt target m) "~e"))
  (define f (lambda a (apply dynamic-send target m a)))
  (apply xcall f #:caller name #:thrown throw-hdler #:timed-out time-out-hdler #:f-msg-format fmt a))

(define (xcall f
               #:caller (name #false)
               #:thrown (throw-handler failed)
               #:timed-out (time-out-handler (λ _ (failed 'time)))
               #:f-msg-format (fmt (string-append (format "xcall: ~a:\n" (object-name f)) "~e"))
               . a)
  (define cust (make-custodian))
  ;; (custodian-limit-memory cust 1048576) ;; memory limit
  (struct okay (value))
  (struct thrw (value))

  (define results-of-thread (make-channel))
  (define th
    (thread
     (lambda ()
       (with-handlers ((void (lambda (x) (channel-put results-of-thread (thrw x)))))
         (define result-of-call (apply f a))
         (channel-put results-of-thread (okay result-of-call))))))
  (define result
    (or (sync/timeout (time-out-limit) results-of-thread)
        (and (kill-thread th) #f)))

  (define is-it-really-me (is-it-me name))

  (cond
    [(okay? result) (okay-value result)]
    [(false? result)
     (log-error (format fmt (~a "time out after " (time-out-limit) " s")))
     (time-out-handler)]
    [(thrw? result)
     (define thrown (thrw-value result))
     (when (is-it-really-me thrown)
       (raise thrown))
     (log-error (format fmt (if (exn? thrown) (exn-message thrown) thrown)))
     (if (show-exn)
         (raise thrown)
         (throw-handler thrown))]
    [else (error 'xdynamic-send "something went horribly wrong: ~e" result)]))

#; {(U False String) -> Any -> Boolean}
(define (is-it-me my-name)
  (define px-my-name (and my-name (pregexp my-name)))
  (if (boolean? px-my-name)
      (lambda (efc) #false)
      (lambda (efc)
        (if (exn:fail:contract? efc)
            (let* ([m (exn-message efc)]
                   [m (regexp-match #px"blaming:(?m:(.*)$)" m)])
              (and m (regexp-match px-my-name (second m))))
            #false))))

;; ---------------------------------------------------------------------------------------------------
(define (error-printing-thread f)
  (define error-printing-channel (make-channel))
  (thread
   (λ ()
     (let loop ()
       (f (channel-get error-printing-channel))
       (loop))))
  error-printing-channel)

;
;
;
;     ;                       ;
;     ;                       ;
;   ;;;;;;   ;;;;    ;;;;   ;;;;;;   ;;;;
;     ;     ;    ;  ;    ;    ;     ;    ;
;     ;     ;;;;;;  ;         ;     ;
;     ;     ;        ;;;;     ;      ;;;;
;     ;     ;            ;    ;          ;
;     ;     ;;   ;  ;    ;    ;     ;    ;
;      ;;;   ;;;;;   ;;;;      ;;;   ;;;;
;
;
;
;

#;(module+ test
  (require (submod ".."))

  (check-equal? '("referee.rkt")
                ((is-it-me "referee.rkt")
                 (exn:fail:contract "blaming: bar/referee.rkt" (current-continuation-marks))))

  (define test%
    (class object%
      (super-new)
      (define/public (good x) GOOD)
      (define/public (better x y) #false)
      (define/public (diverge x y z) (let loop () (loop)))
      (define/public (raise-exn w) (raise 0))))

  (define test (new test%))

  (define GOOD 5)
  (struct ex [value])
  (struct tt [])

  (time-out-limit .001)
  (check-equal? (xdynamic-send test 'good #:thrown ex #:timed-out tt 0) GOOD)
  (check-equal? (xdynamic-send test 'better #:thrown ex #:timed-out tt 0 1) #f)
  (check-pred   tt? (xdynamic-send test 'diverge #:thrown ex #:timed-out tt 0 1 2))
  (check-pred   ex? (xdynamic-send test 'raise-exn #:thrown ex #:timed-out tt 3)))
