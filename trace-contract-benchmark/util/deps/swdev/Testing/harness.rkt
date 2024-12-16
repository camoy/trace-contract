#lang racket

;; support for the three major ways of running tests in Sw Dev

;; The names of the JSON testfiles is determined by the following functions: 

(define (recognize-input-filename fn)
  (regexp-match #px"^(.*/)?([^/]+-)?([0-9]+)-in.json$" fn))

(define (matching-output-file-name prefix numberstr)
  (format "~a~a-out.json" (or prefix "") numberstr))

;; Normally, we just use <n>-in.json and <n>-out.json for n = 0, 1, 2, ...

;; TODO:
;; -- client-server would benefit from functions that look at the _entire_ test input and select it
;; -- ... would benefit from a #:cmd argument to which the port number is _added_ 
;; -- doc for #:check after figuring out what it checks
 
;; ---------------------------------------------------------------------------------------------------
(provide
 (contract-out
  [client
   
   #; [(client #:check c #:cmd cl #:inexact-okay? t #:tcp p)
       path-to-test-inputs path-to-client-executable]
   
   ;; runs `path-to-client-executable` as a parallel client process with command-line cl 
   ;; feeding it the JSexprs in every test input file from `path-to-test-inputs` one at a time
   ;; on TCP port p, if specified, in which case the harness is the server.

   ;; The harness compares the actual output of `path-to-client-executable` with the one expected
   ;; output in <n>-out. The comparison is JSexpr-based not text-based. 
   ;;
   ;; If t is #false, JSON numbers must be exactly equal;
   ;; otherwise they are compare with tolerance t.
   
   (->i (#:check [valid-json (-> any/c any)])
        (#:cmd   (command-line-args (listof string?))
         #:tcp   (tcp-on (or/c #false (and/c (>/c 1024) (</c 65000))))
         #:inexact-okay? [ok (or/c #false (and/c real? (</c 1.0) (>/c 0.0)))])
        (r (->i ([test-directory-path path-string?]
                 [client-to-be-tested path-string?])
                [r any/c])))]
  
  [server
   ;; like client, but the given exec plays the role of TCP server if #:tcp PORT comes along
   ;; and the harness takes on the role of client 
   (->i (#:check [valid-json (-> any/c any)])
        (#:cmd   (command-line-args (listof string?))
         #:tcp   (tcp-on (or/c #false (and/c (>/c 1024) (</c 65000))))
         #:inexact-okay? [ok (or/c #false (and/c real? (</c 1.0) (>/c 0.0)))])
        (r (->i ([test-directory-path path-string?]
                 [server-to-be-tested path-string?])
                [r any/c])))]

  [client-and-server
   
   #; [(client-and-server #:check c #:inexact-okay? t #:prepare-server ps #:prepare-client pc)
       path-to-test-inputs path-to-client-executable path-to-server-executable]
        
   ;; runs `path-to-client-executable` and `path-to-server-executable` as parallel processes
   ;; feeding each the JSexpr in every test input file from `path-to-test-inputs` one at a time
   ;; but deciding whether it should be fed into the client or server with (pc i) and (ps i),
   ;; respectively. The two processes communicate with each other via TCP on an generated port.
   ;; 
   ;; The harness compares the actual output from the server with the one expected in <n>-out.
   ;; The comparison is JSexpr-based not text-based.
   ;;
   ;; If t is #false, JSON numbers must be exactly equal;
   ;; otherwise they are compare with tolerance t.
   
   (->i (#:check          [valid-json (-> jsexpr? any/c)])
        (#:inexact-okay?  [tolerance (or/c #false (between/c .0 .1))]
         #:prepare-server [ps (-> natural? boolean?)]
         #:prepare-client [pc (-> natural? boolean?)])
        (r (->i ([test-directory-path path-string?]
                 [client-to-be-tested path-string?]
                 [server-to-be-tested path-string?])
                (r any/c))))]

  [unset-time-out
   ;; sets the time-out limit to `limit` seconds; 1000 is default
   ;; from "communications.rkt"
   (->i () (#:limit (limit natural?)) (r any/c))])

 ;; [Parameter Boolean]
 test-plain-and-pretty-json?              ;; preset by default 
 test-fast-and-slow-delivery?             ;; the remaining are unset 
 test-with-and-without-trailing-newline? 
 test-with-and-without-escaped-unicode?
 test-the-first-n-at-most                 ;; how many of the tests will be used, maximally
 test-with-batch-mode?)

;                                                                                      
;       ;                                  ;                                           
;       ;                                  ;                          ;                
;       ;                                  ;                                           
;    ;;;;   ;;;   ;;;;    ;;;   ; ;;    ;;;;   ;;;   ; ;;    ;;;    ;;;    ;;;    ;;;  
;   ;; ;;  ;;  ;  ;; ;;  ;;  ;  ;;  ;  ;; ;;  ;;  ;  ;;  ;  ;;  ;     ;   ;;  ;  ;   ; 
;   ;   ;  ;   ;; ;   ;  ;   ;; ;   ;  ;   ;  ;   ;; ;   ;  ;         ;   ;   ;; ;     
;   ;   ;  ;;;;;; ;   ;  ;;;;;; ;   ;  ;   ;  ;;;;;; ;   ;  ;         ;   ;;;;;;  ;;;  
;   ;   ;  ;      ;   ;  ;      ;   ;  ;   ;  ;      ;   ;  ;         ;   ;          ; 
;   ;; ;;  ;      ;; ;;  ;      ;   ;  ;; ;;  ;      ;   ;  ;;        ;   ;      ;   ; 
;    ;;;;   ;;;;  ;;;;    ;;;;  ;   ;   ;;;;   ;;;;  ;   ;   ;;;;   ;;;;;  ;;;;   ;;;  
;                 ;                                                                    
;                 ;                                                                    
;                 ;                                                                    

(require "communication.rkt")
(require "port-objects.rkt")
(require "tcp.rkt")
(require "../Lib/json-equal.rkt")
(require json)
(require (for-syntax racket/syntax))

(module+ test
  (require rackunit))

;                                                                        
;                                                                        
;                                               ;                        
;                                               ;                        
;   ;;;;   ;;;;    ;;;;  ;;;;  ;;;;;;   ;;;   ;;;;;   ;;;    ;;;;   ;;;  
;   ;; ;;      ;   ;;  ;     ; ;  ;  ; ;;  ;    ;    ;;  ;   ;;  ; ;   ; 
;   ;   ;      ;   ;         ; ;  ;  ; ;   ;;   ;    ;   ;;  ;     ;     
;   ;   ;   ;;;;   ;      ;;;; ;  ;  ; ;;;;;;   ;    ;;;;;;  ;      ;;;  
;   ;   ;  ;   ;   ;     ;   ; ;  ;  ; ;        ;    ;       ;         ; 
;   ;; ;;  ;   ;   ;     ;   ; ;  ;  ; ;        ;    ;       ;     ;   ; 
;   ;;;;    ;;;;   ;      ;;;; ;  ;  ;  ;;;;    ;;;   ;;;;   ;      ;;;  
;   ;                                                                    
;   ;                                                                    
;   ;                                                                    

#; {type Setup = [-> (values InputPort OutputPort [-> Void])]}

(define test-plain-and-pretty-json?             (make-parameter #t))
(define test-fast-and-slow-delivery?            (make-parameter #f))
(define test-with-and-without-trailing-newline? (make-parameter #f))
(define test-with-and-without-escaped-unicode?  (make-parameter #f))
(define test-with-batch-mode?                   (make-parameter #f))
(define test-the-first-n-at-most                (make-parameter +inf.0))

;; This is to make the external parameter names consistent with Tony's implementation 
(define-syntax (def/setting stx)
  (syntax-case stx ()
    [(_ para pos neg)
     (with-syntax ([test-para (format-id stx "test-~a" #'para)])
       #'(define para (make-parameter (lambda () (if (test-para) pos neg)))))]))

(def/setting plain-and-pretty-json?             '(#f #t) '(#f))
(def/setting fast-and-slow-delivery?            '(#f #t) '(#f))
(def/setting with-and-without-trailing-newline? '(#t #f) '(#t))
(def/setting with-and-without-escaped-unicode?  '(#f #t) '(#f))

(define json-precision (make-parameter #false))

(define ACCEPT-TIMEOUT 5)    ;; seconds. See (server).
(define RETRY-COUNT    10)   ;; with a retry every .5 sec. See (client).


;                                                                                                    
;                                                                                                    
;                                                                  ;;;       ;                   ;   
;                                                                    ;                           ;   
;    ;;;    ;;;    ;;;;  ;   ;   ;;;    ;;;;                 ;;;     ;     ;;;    ;;;   ; ;;   ;;;;; 
;   ;   ;  ;;  ;   ;;  ; ;   ;  ;;  ;   ;;  ;               ;;  ;    ;       ;   ;;  ;  ;;  ;    ;   
;   ;      ;   ;;  ;      ; ;   ;   ;;  ;                   ;        ;       ;   ;   ;; ;   ;    ;   
;    ;;;   ;;;;;;  ;      ; ;   ;;;;;;  ;                   ;        ;       ;   ;;;;;; ;   ;    ;   
;       ;  ;       ;      ; ;   ;       ;                   ;        ;       ;   ;      ;   ;    ;   
;   ;   ;  ;       ;       ;    ;       ;       ;;          ;;       ;       ;   ;      ;   ;    ;   
;    ;;;    ;;;;   ;       ;     ;;;;   ;       ;;           ;;;;     ;;   ;;;;;  ;;;;  ;   ;    ;;; 
;                                               ;                                                    
;                                              ;;                                                    
;                                                                                                    

(define ((server #:check valid-json
                 #:cmd   (cmd '())
                 #:tcp   (tcp #false)
                 #:inexact-okay? [p 0.001])
         tests-directory-name client-to-be-tested)
  
  #; {InputPort OutputPort -> (values InputPort OutputPort)}
  ;; deliver two ports on which communication with the client happens 
  (define (connect stdout stdin)
    (define listener (tcp-listen (or tcp REMOTE-PORT0) 30 #true))
    (cond
      [(not tcp) (values stdout stdin)]
      [(sync/timeout ACCEPT-TIMEOUT listener) => (λ (l) (channel2 ports->objects (tcp-accept l)))]
      [else
       (raise-connection-error "failed to accept a connection within ~a seconds" ACCEPT-TIMEOUT)]))
  
  (json-precision p)
  (define setup (make-setup client-to-be-tested cmd connect))
  (work-horse setup client-to-be-tested tests-directory-name valid-json))

(define ((client #:check valid-json
                 #:cmd   (cmd '())
                 #:tcp   (tcp #false)
                 #:inexact-okay? [p 0.001])
         tests-directory-name server-to-be-tested)
  
  #; {InputPort OutputPort -> (values InputPort OutputPort)}
  ;; deliver two ports on which communication with the server happens 
  (define (connect from to)
    (if tcp (channel2 ports->objects (try-to-connect-to-times RETRY-COUNT tcp)) (values from to)))

  (json-precision p)
  (define setup (make-setup server-to-be-tested cmd connect))
  (work-horse setup server-to-be-tested tests-directory-name valid-json))

;; ---------------------------------------------------------------------------------------------------
(define ((client-and-server #:check valid-json
                            #:inexact-okay?  [p 0.001]
                            #:prepare-client [pc values]
                            #:prepare-server [ps values])
         tests-directory-name client-to-be-tested server-to-be-tested)
  (json-precision p)
  (define setup [make-setup-server-client pc client-to-be-tested ps server-to-be-tested])
  (work-horse setup (~a client-to-be-tested " " server-to-be-tested) tests-directory-name valid-json))

#;{([Listof JSexpr] -> Any)
   ProgString
   ([Listof JSexpr] -> Any)
   ProgString
   ->
   (values [Instanceof InputPort] [Instanceof OutputPort] (-> Void))}
;; combinet the two set-up thunks into one, with a combined port for feeding both test case inputs
;; also return a function for tearing down resources 
(define ([make-setup-server-client pc client-to-be-tested ps server-to-be-tested])

  (define cmd (list (~a (get-starter-port))))
  (define server-setup (make-setup server-to-be-tested cmd values))
  (define client-setup (make-setup client-to-be-tested cmd values))
  (define-values (from-server to-server server-tear-down) [server-setup])
  (define-values (from-client to-client client-tear-down) [client-setup])

  (define in from-server)
  (define out (combine-output-ports to-server to-client ps pc))
  (define (tear-down)
    (define client-shut-down (thread (λ () (client-tear-down))))
    (server-tear-down)
    (sync client-shut-down))

  (values in out tear-down))

;; ---------------------------------------------------------------------------------------------------
#;
[client/no-tests
 ;; run _to-be-tested_ on test inputs from STDIN 
 ;; #:tcp determines whether the communication uses TCP; STDIN/STDOUT is default 
 ;; #:check can be used to determine the validity of the JSON input
 ;;         there is also a well-formedness check (but it is diabled)
 #; (-> [List FileName [Listof JSexpr] FileName [Listof JSexpr]] Boolean)
   
 #; (make-client-server-contract)
 (->i ()
      (#:stdin (stdin path-string?)   ;; a file to be re-directed into `to-be-tested`
       #:cmd   (cmd (listof string?)) ;; command line arguments 
       #:tcp   (tcp-on (or/c #false (and/c (>/c 1024) (</c 65000)))))
      (r (->i ([to-be-tested path-string?]) [r any/c])))]

(define ((client/no-tests #:cmd   (cmd '())
                          #:tcp   (tcp #f)
                          #:stdin (stdin #f))
         program-to-be-tested)
  (define (x stdout stdin) (if tcp (try-to-connect-to-times RETRY-COUNT tcp) (values stdout stdin)))
  (define setup (make-setup program-to-be-tested cmd x #:stdin stdin))
  (work-horse/no-tests setup program-to-be-tested))

;; ---------------------------------------------------------------------------------------------------
#;
(make-setup ;; I exported this for ~/Course/20SwDev/
 #; (PathString [Listof String] [InputPort OutputPort -> (values InputPort OutputPort)] -> Setup)
 #; (define (x stdout stdin) (if tcp (try-to-connect-times RETRYCOUNT tcp) (values stdout stdin)))
 #; (make-setup "./xfoo" (list "10" "45678") x)
 ;; sets up `xfoo`to run as a subprocess (group),
 ;; applies x to the STDOUT and STDIN ports, which are the ports for "us" to read outputs of `xfoo`
 ;;    or feed it inputs
 ;; creates a thunk for tearing down the process group 
 (->i ([program-to-be-tested path-string?]
       [cmd-line-flags       (listof string?)]
       [align-in-out         (-> input-port? output-port? (values input-port? output-port? ))])
      (r (-> (values input-port? output-port? (-> any/c))))))

#; ([PathString [Listof String] InputPort OutputPort -> (values InputPort OutputPort)] -> Setup)
(define (make-setup test-program args f #:stdin (config #f))
  (define (setup)
    (define custodian (make-custodian))
    (parameterize ((current-custodian custodian)
                   (current-subprocess-custodian-mode 'kill)
                   (subprocess-group-enabled
                    (or (eq? (system-type) 'unix) (eq? (system-type) 'macosx))))
      
      #;{InputPort OutputPort ProcessId OutputPort (Symbol -> Any)}
      (define-values (from0 to0 pid _stderr query) (apply spawn test-program args))
      (define-values (from-program to-program) (channel2 ports->objects (values from0 to0)))

      (when config
        (with-input-from-file config
          (lambda ()
            (define lines (port->lines))
            (flush-output to-program))))
      
      (define (tear-down)
        (kill-process pid query)
        (custodian-shutdown-all custodian))
      
      (define-values (in out) (f from-program to-program))
      (values in out tear-down)))
  setup)

;; ---------------------------------------------------------------------------------------------------
#; (PathString [Any ...] -> Void)
(define (spawn command . args)
  (apply values (apply process*/ports #f #f (current-error-port) command args)))

;; ProcessId -> Void 
(define (kill-process pid query)
  (query 'kill)
  (query 'wait))

;; ---------------------------------------------------------------------------------------------------
(struct exn:fail:connection exn:fail ())
(define (raise-connection-error msg . args)
  (raise (exn:fail:connection (apply format msg args) (current-continuation-marks))))

#; (N Port -> (values InputPort OutputPort))
(define (try-to-connect-to-times retry-limit tcp)          
  (define-values (in out)
    (let retry ((count 1))
      (cond
        [(> count retry-limit)
         (raise-connection-error "no connection after ~a tries" retry-limit)]
        [else 
         (sleep 0.5)
         (with-handlers ((exn:fail:network? (lambda (x) (retry (+ 1 count)))))
           (tcp-connect LOCALHOST tcp))])))
  (values in out))

;                                                                 
;                        ;      ;                                 
;                        ;      ;                                 
;                        ;      ;                                 
;  ;     ;  ;;;    ;;;;  ;  ;   ; ;;    ;;;    ;;;;   ;;;    ;;;  
;  ;     ; ;; ;;   ;;  ; ;  ;   ;;  ;  ;; ;;   ;;  ; ;   ;  ;;  ; 
;   ; ; ;  ;   ;   ;     ; ;    ;   ;  ;   ;   ;     ;      ;   ;;
;   ; ; ;  ;   ;   ;     ;;;    ;   ;  ;   ;   ;      ;;;   ;;;;;;
;   ;; ;;  ;   ;   ;     ; ;    ;   ;  ;   ;   ;         ;  ;     
;   ;; ;;  ;; ;;   ;     ;  ;   ;   ;  ;; ;;   ;     ;   ;  ;     
;    ; ;    ;;;    ;     ;   ;  ;   ;   ;;;    ;      ;;;    ;;;; 
;                                                                 
;                                                                 
;                                                                 

#; (Setup String JSONCheck -> Void)
(define (work-horse/no-tests setup program-to-be-tested)
  (displayln `(running ,program-to-be-tested))
  (for* ([pretty     ([plain-and-pretty-json?])]
         [trickle    ((fast-and-slow-delivery?))]
         [terminated ([with-and-without-trailing-newline?])]
         [escaped    ((with-and-without-escaped-unicode?))])
    (match-define `(,n ,config) (test-one/no-tests pretty trickle terminated escaped setup))
    (displayln `(ran ,program-to-be-tested successfully (,config) and received ,n JSON values))))

#;(Boolean Boolean Boolean Boolean [-> Setup] -> (List N Configuration))
(define (test-one/no-tests pretty trickle terminated escaped setup)
  (define pretty-sym (if pretty 'pretty 'plain))
  (define trickle-sym (if trickle 'slow 'fast))
  (define terminated-sym (if terminated 'with-newline 'with-space))
  (define escaped-sym (if escaped 'escaped-unicode 'plain-unicode))
  (define classification (list pretty-sym trickle-sym terminated-sym escaped-sym))
  (log-info " ... ~v" classification)
  (define-values (in out tear-down) (setup))
  (define actual 
    (parameterize ((pretty-print-output? pretty)
                   (trickle-output?      trickle)
                   (trailing-newline?    terminated)
                   (encode-all-unicode?  escaped))
      (read-rest in)))
  (tear-down)
  (log-info "received ~v" actual)
  (list (length actual) classification))

(define (display-results all-tests results total-test-count)
  (displayln
   (filter (lambda (x) x)
           (for/list ([t all-tests]
                      [r results])
             (match-define `(,in-fname ,input* ,out-fname ,expected-out) t)
             (and (= r 1)
                  (list in-fname out-fname)))))
  (displayln
   `((passed ,(count (lambda (v) (= v 1)) results))
     (total ,total-test-count)
     (partial-score ,(apply + results)))))

;; ---------------------------------------------------------------------------------------------------
#; (Setup String Path JSONCheck -> Void)
(define (work-horse setup program-to-be-tested tests-directory-name valid-json?)
  (file-stream-buffer-mode (current-output-port) 'line)
  (file-stream-buffer-mode (current-error-port) 'line)
  (parameterize ()
    (displayln `(testing ,program-to-be-tested))
    
    (define file*       (json-test-files (in-directory tests-directory-name (lambda (_path) #f))))
    (define test*       (retrieve-all-tests file*))
    (define all-tests#  (length test*))
    (define valid-tests (eliminate-bad-tests valid-json? test*))
    

    (with-handlers ([exn:fail:connection? (lambda (e)
                                            (displayln (exn-message e))
                                            (display-results '() '() all-tests#))])
      (define results (test-them setup valid-tests))
      (display-results valid-tests results all-tests#))))

#;(Setup [Listof TestSpec] -> (List Score))
(define (test-them setup all-tests)
  (for/list ((t all-tests) (i-th-test (in-naturals)))
    (match-define `(,in-fname ,input* ,out-fname ,expected-out) t)
    (displayln `(testing ,in-fname ,out-fname) (current-error-port))
    (define actual-output
      (for*/list ([pretty     ([plain-and-pretty-json?])]
                  [trickle    ((fast-and-slow-delivery?))]
                  [terminated ([with-and-without-trailing-newline?])]
                  [escaped    ((with-and-without-escaped-unicode?))])
        (test-one pretty trickle terminated escaped setup input*)))
    (compare input* expected-out actual-output)))

#; (Boolean Boolean Boolean Boolean [-> Setup] [Listof JSexpr]
            ->
            [List [List Symbol Symbol Symbol Symbol] JSexpr])
;; run a single test in the specified context, restart the to-be-tested program
(define (test-one pretty trickle terminated escaped setup input*)
  (define pretty-sym (if pretty 'pretty 'plain))
  (define trickle-sym (if trickle 'slow 'fast))
  (define terminated-sym (if terminated 'with-newline 'with-space))
  (define escaped-sym (if escaped 'escaped-unicode 'plain-unicode))
  (define classification (list pretty-sym trickle-sym terminated-sym escaped-sym))
  (log-info " ... ~v" classification)
  (define-values (in out tear-down) (setup))
  (define actual 
    (parameterize ((pretty-print-output? pretty)
                   (trickle-output?      trickle)
                   (trailing-newline?    terminated)
                   (encode-all-unicode?  escaped))
      (if (and in out)
          (feed-and-receive in out input*)
          'failed-to-establish-connection)))
  (log-info "received actual ... ~v" actual)
  (tear-down)
  (list classification actual))

#;{ type TestSpec = [List FileName JSexpr FileName JSexpr]}

#;([Listof TestSpec] [TestSpec -> (or/c TestSpec #f)] -> [Listof TestSpec])
(define (eliminate-bad-tests valid-json? test*)
  (filter-map (make-exn-safe valid-json?) (filter test-with-wff-json? test*)))

#; ([Listof [List FileName FileName]] -> [Listof TestSpec])
(define (retrieve-all-tests file*)
  (for/list ((x file*) (_i (in-naturals)) #:when (< _i (test-the-first-n-at-most)))
    (match-define `(,in-fname ,out-fname) x)
    (match-define `(,input ,output) (list (file->json in-fname) (file->json out-fname)))
    (list in-fname input out-fname output)))

;; ---------------------------------------------------------------------------------------------------
#; (InputPort OutputPort [Listof JSexpr] -> [Listof JSexpr])
(define (feed-and-receive in out input*)
  (define batch? (test-with-batch-mode?))
  (parameterize ()
    (write-and-read batch? input* in out)))

#; {[Listof JSexpr] -> [Listof JSexpr]}
;; EFFECT
;;  (1) send all inputs to the input port
;;  (2) if interactive, read response for each input
;;  (3) in any case, when all inputs are sent, close port and read all responses 
(define (write-and-read batch? remaining-inputs0 in out)
  (let write-and-read ([remaining-inputs remaining-inputs0])
    (match remaining-inputs
      ['()
       ; We're pretty sure the need for this is a bug in Racket's TCP port handling.
       (with-handlers ([exn:fail:network? (lambda (e) (void))])
         (send out close))
       (read-rest in)]
      [(cons i rest)
       (send out message i)
       (if batch?
           (write-and-read rest)
           (match (send in read)
             [(? eof-object?) '()]
             [(? terminal-value? v) (list v)]
             [v (cons v (write-and-read rest))]))])))

#; [-> [Listof JSexpr]]
;; EFFECT keep reading until current output port is closed 
(define (read-rest in)
  (define next (send in read))
  (match next
    [(? eof-object?) '()]
    [(? terminal-value? v) (list v)]
    [v (cons v (read-rest in))]))

;; ---------------------------------------------------------------------------------------------------
(define (make-exn-safe test-pred)
  (lambda (t)
    (match-define `(,in-fname ,_ ,_ ,_) t)
    (define ok?
      (with-handlers [(exn? (lambda (e)
                              (local-require racket/exn)
                              (define str (exn->string e))
                              (log-error "Test validation for ~e failed:\n~a" in-fname str)
                              #f))]
        (test-pred t)))
    (when (not ok?) (displayln `(INVALID-TEST ,in-fname)))
    ok?))

; Check that inputs & outputs are lists, not #f; all-json-expressions gives #f when invalid json
(define (test-with-wff-json? t)
  (match t
    [(list _in-fname (? list?) _out-fname (? list?)) #t]
    [_ #f]))

;                                                   
;                                                   
;                                                   
;                                                   
;    ;;;    ;;;  ;;;;;;  ;;;;   ;;;;    ;;;;   ;;;  
;   ;;  ;  ;; ;; ;  ;  ; ;; ;;      ;   ;;  ; ;;  ; 
;   ;      ;   ; ;  ;  ; ;   ;      ;   ;     ;   ;;
;   ;      ;   ; ;  ;  ; ;   ;   ;;;;   ;     ;;;;;;
;   ;      ;   ; ;  ;  ; ;   ;  ;   ;   ;     ;     
;   ;;     ;; ;; ;  ;  ; ;; ;;  ;   ;   ;     ;     
;    ;;;;   ;;;  ;  ;  ; ;;;;    ;;;;   ;      ;;;; 
;                        ;                          
;                        ;                          
;                        ;                          

;; [Listof JSexpr] [Listof JSexpr] [Listof (List Symbol (Listof JSexpr))] -> Symbol
;; compare: 'ok for the expected equals actual; 'partial for some passes; 'fail otherwise
;; effect: write out diff for failed test
(define (compare input* expected-out actual-outputs)
  (define number-outputs (length actual-outputs))
  (define partial-score  (/ number-outputs))
  (define score (for/sum [(entry actual-outputs)]
                  (match-define (list _classification actual-out) entry)
                  (if (compare-expected-actual expected-out actual-out) partial-score 0)))

  (when (not (= score 1))
    (displayln '---------------------------------)
    (displayln `(*** score ,score))
    (displayln `(*** on))
    (pretty-print input*)
    (displayln '(*** expected))
    (pretty-print expected-out)
    (displayln `(*** but received))
    (pretty-print actual-outputs)
    (displayln "\n"))

  score)

#; {[Listof JSExpr] [Listof JSExpr] -> Booleaan}

(module+ test
  (json-precision 0.001)
  (check-true (compare-expected-actual 3.0 3))
  
  (check-true (compare-expected-actual #f #f))
  (check-false (compare-expected-actual #f 3))

  (check-true (compare-expected-actual (json-null) (json-null)))

  (check-true (compare-expected-actual "🤪" "🤪"))
  (check-false (compare-expected-actual "🤪" "🤪 hello"))

  (check-true (compare-expected-actual (hash 'a 3.0 'b "hello world") (hash 'b "hello world"  'a 3)))
  (check-true
   (compare-expected-actual (list "a" 3.0 "b" "hello world") (list "a" 3 "b" "hello world")))

  (check-false (compare-expected-actual cons cons) "the function dispatches on JSON only")
  (check-false (compare-expected-actual 'a 'a) "the function dispatches on JSON only"))

(define (compare-expected-actual expected-out actual-out)
  (or (json-equal? expected-out actual-out #:inexact-okay? (json-precision))
      (match* (expected-out actual-out)
        [((list (? string? expected-single)) (list (? string? actual-single)))
         (regexp-match expected-single actual-single)]
        [(_ _) #false])))

;; -----------------------------------------------------------------------------
#; {String -> [Maybe [Listof JSexpr]]}
;; read f as JSON file if possible 
;; effect: display its name if it is not
(define (file->json f)
  (with-input-from-file f (all-json-expressions f)))

;                                     
;                                     
;     ;                    ;          
;     ;                    ;          
;   ;;;;;   ;;;    ;;;   ;;;;;   ;;;  
;     ;    ;;  ;  ;   ;    ;    ;   ; 
;     ;    ;   ;; ;        ;    ;     
;     ;    ;;;;;;  ;;;     ;     ;;;  
;     ;    ;          ;    ;        ; 
;     ;    ;      ;   ;    ;    ;   ; 
;     ;;;   ;;;;   ;;;     ;;;   ;;;  
;                                     
;                                     
;                                     

(module+ test
  (check-equal? (with-input-from-string "1" (all-json-expressions "nonsense"))
                '(1)
                "true good file")
  
  (check-equal? (with-output-to-string
                  (lambda ()
                    (with-input-from-string "1" (all-json-expressions "nonsense"))))
                ""
                "frame condition: no effect for good file")
  
  (check-equal? (let ([s (open-output-string)])
                  (parameterize ([current-error-port s])
                    (with-input-from-string "()" (all-json-expressions "nonsense")))
                  (get-output-string s))
                "nonsense contains something other than JSON\n"
                "write for bad file")
  
  (check-equal?
   (parameterize ([current-error-port (open-output-string)])
     (with-input-from-string "()" (all-json-expressions "nonsense")))
   #false
   "false for bad file")

  (check-equal?
   (parameterize ([current-error-port (open-output-string)])
     (eliminate-bad-tests
      values
      (list
       (list "1-in.json"
             (list "foo")
             "1-out.json"
             (with-input-from-string "unquoted string" (all-json-expressions "nonsense"))))))
   '()))

(define ((all-json-expressions f))
  (let all-json-lines ([seen-so-far '()])
    (define next (read-message))
    (cond
      [(eof-object? next) (reverse seen-so-far)]
      [(terminal-value? next) (eprintf "~a contains something other than JSON\n" f) #false]
      [else (all-json-lines (cons next seen-so-far))])))

;; -----------------------------------------------------------------------------
;; [Listof Path] -> [Listof [List String String]]
;; Find all pairs of the form x-i-in.json and x-i-out.json or
;; i-in.json and i-out.json for i in Nat+ and for all x in file name
;; prefixes.

(module+ test
  
  (check-equal? (json-test-files '()) '())
  
  (define path0 (list (build-path "1-in.json") (build-path "1-out.json")))
  (define strg0 (list (map path->string path0)))
  (check-equal? (json-test-files path0) strg0)
  
  (define path1 (list (build-path "1-in.json") (build-path "2-out.json")))
  (check-equal? (json-test-files path1) '())

  (define path2 (list (build-path "/foo/bar/1-in.json")
                      (build-path "/foo/bar/1-out.json")
                      (build-path "/foo/bar/zot-1-in.json")
                      (build-path "/foo/bar/zot-1-out.json")
                      (build-path "/foo/bar/foo-bar-1-in.json")
                      (build-path "/foo/bar/foo-bar-1-out.json")
                      (build-path "/foo/bar/quux-2-in.json")
                      (build-path "/foo/bar/quux-3-out.json")
                      (build-path "/foo/bar/foo-bar-2-in.json")
                      (build-path "/foo/bar/foo-bar-3-out.json")
                      (build-path "/foo/bar/2-out.json")
                      (build-path "/foo/bar/3-in.json")))
  (check-equal? (json-test-files path2)
                (list (list "/foo/bar/1-in.json" "/foo/bar/1-out.json")
                      (list "/foo/bar/foo-bar-1-in.json" "/foo/bar/foo-bar-1-out.json")
                      (list "/foo/bar/zot-1-in.json" "/foo/bar/zot-1-out.json")))
  
  (check-equal? (json-test-files (append path0 path1)) strg0))

#; {[Listof PathString] -> [Listof [List PathString PathString]]}
(define (json-test-files d)
  #; [Listof PathString]
  ;; such that their suffix is '.json'
  (define all-json-files
    (for/list ((f d)  #:when #px"^(.*/)?([^/]+-)?[0-9]+-[^/]*.json$")
      (path->string f)))

  #; [listof [List PathString PathString]]
  (define the-files
    (filter-map
     (lambda (input-filename)
       (cond
         [(recognize-input-filename input-filename)
          =>
          (match-lambda
            [(list _entire _dir prefix numberstr)
             (define matched-name    (matching-output-file-name prefix numberstr))
             (define output-filename (path-with-file-named all-json-files matched-name))
             (and output-filename (list input-filename output-filename))])]
         [else #f]))
     all-json-files))

  (define -duplicates (remove-duplicates the-files))

  (sort -duplicates string<? #:key car))

#; {[Listof PathString] String -> PathString}
;; find the full path string for the file named `x` :: guaranteed due to call site 
(define (path-with-file-named all-json-files x)
  (findf (lambda (f) (and (>= (string-length f) (string-length x))
                          (equal? (substring f
                                             (- (string-length f) (string-length x))
                                             (string-length f))
                                  x)
                          (or (= (string-length f) (string-length x))
                              (char=? (string-ref f (- (string-length f) (string-length x) 1))
                                      #\/))))
         all-json-files))

#|

When you use `system` or `process`, the immediate new process runs a
shell. The shell process then starts another one to run the command
that you give it.

I recommend using `process*` to avoid the shell process and to avoid
encoding issues when passing arguments:

(define racket (find-executable-path "racket"))
... (process* racket "sleeper.rkt") ...

Another approach is to create a fresh process group for the shell
process, and then `((fifth pl) 'kill)` kills the whole group:

 (parameterize ([subprocess-group-enabled #t])
   (process "racket sleeper.rkt"))

Finally, you could tell the shell to not create a subprocess and
instead replace itself with the other program"

 (process "exec racket sleeper.rkt")

|#
