#lang racket

;; provide a utility for rotating thru ports plus some basic TCP constants 

(provide 
 LOCALHOST
 REMOTE-PORT0
 
 (contract-out
  ;; consult a PORT-STARTER-FILE and get the next one
  ;; RACE CND it suffers from the usual race condition (someone can grab the port _after_ return)
  ;; EFFECT Create a starter-file in the users ~/Tmp directory
  ;; EXN this will fail when all ~40,000 ports above BASE (2345) are in use 
  (get-starter-port (-> port-number?))))

; ---------------------------------------------------------------------------------------------------
(define LOCALHOST    "127.0.0.1")
(define REMOTE-PORT0 23456)

(define HOME (find-system-path 'home-dir))
(define TEMP (build-path HOME "Tmp/"))
(unless (directory-exists? TEMP) (make-directory TEMP))
(define PORT-STARTER-FILE (build-path TEMP "port-starter-file.rktd"))

;; ---------------------------------------------------------------------------------------------------
(define (get-starter-port)
  (define p0 (retrieve-port-from-starter-file))
  (define p (find-port-not-in-use p0))
  (write-port-to-starter-file p)
  p)

#; {PortNumber -> PortNumber}
;; search for a tcp port that is currently not in use
(define (find-port-not-in-use p0)
  (let search ([p p0])
    (with-handlers ([exn:fail:network? (λ (xn) (log-error "port in use: ~a" p) (search (+ p 1)))])
      (parameterize ([current-custodian (make-custodian)])
        (define _ (tcp-listen p 30 #true))
        (begin0 p
                (custodian-shutdown-all (current-custodian)))))))

#; {-> PortNumber}
(define (retrieve-port-from-starter-file)
  (cond
    [(file-exists? PORT-STARTER-FILE)
     (define candidate (with-input-from-file PORT-STARTER-FILE read))
     (if (port-number? candidate) candidate REMOTE-PORT0)]
    [else REMOTE-PORT0]))

#; {PortNumber -> Void}
(define (write-port-to-starter-file p)
  (with-output-to-file PORT-STARTER-FILE (λ () (writeln (add1 p))) #:exists 'replace))