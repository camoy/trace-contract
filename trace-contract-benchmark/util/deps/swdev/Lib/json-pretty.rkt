#lang racket

;; ---------------------------------------------------------------------------------------------------
;; Pretty-printing of JSON.
;; TODO:
;; -- makde thread-safe first (see global set!) then 
;; -- submit this for inclusion in racket.

(provide
 ; (-> 
 ;  jsexpr?  
 ;  [o (current-output-port)]
 ;  #:null [jsnull (json-null)]
 ;  #:encode [enc 'control]
 ;  #:indent-maps? [indent-maps? #t]
 ;  #:indent-lists? [indent-lists? #f]
 ;  #:indent-increment [indent-increment 2]
 ;  void?)
 ; pretty write a JSexpr 
 write-json/pretty)

;; ---------------------------------------------------------------------------------------------------
(require json)

(module+ test
  (require rackunit))

;; ---------------------------------------------------------------------------------------------------
(define (write-json/pretty x
                           [o (current-output-port)]
                           #:null [jsnull (json-null)]
                           #:encode [enc 'control]
                           #:indent-maps? [indent-maps? #t]
                           #:indent-lists? [indent-lists? #f]
                           #:indent-increment [indent-increment 2])
  (set! *indent-level 0)
  (void ;; to blank out the return status of write-byte; otherwise we get 1
  (write-json/pretty* 'write-json/pretty
                      x
                      o
                      jsnull
                      enc
                      indent-maps?
                      indent-lists?
                      indent-increment)))

(define (write-json/pretty* who x o jsnull enc indent-maps? indent-lists? indent-increment)
  (let loop ([x x])
    (cond
      [(or (exact-integer? x) (real-real? x)) (write x o)]
      [(eq? x #f)     (write-bytes #"false" o)]
      [(eq? x #t)     (write-bytes #"true" o)]
      [(eq? x jsnull) (write-bytes #"null" o)]
      [(string? x)    (write-json-string x o)]
      [(list? x)
       (define inside (block y (in-list x) indent-lists? (loop y) o))
       (write-json-block #"[" #"]" indent-lists? indent-increment o inside)]
      [(hash? x)
       (define inside
         (block [k v] (in-hash x) indent-maps? (loop v) o
                ;; MF: I dropped the error check on k. This could be re-introduced here w/o loss.
                ;; use a string encoding so we get the same deal with `rx-to-encode'
                (write-json-string (symbol->string k) o #:encode enc)
                (write-bytes (if indent-maps? #": " #":") o)))
       (write-json-block #"{" #"}" indent-maps? indent-increment o inside)]
      [else (raise-type-error who "legal JSON value" x)])))

;; Nat
;; keep track of how many blank spaces start a line 
(define *indent-level 0)

;; Byte Byte [-> Void] -> Void
;; EFFECT *indent-level 
(define (write-json-block open close when? indent-increment o inside)
  (write-bytes open o)
  (when when? (set! *indent-level (+ *indent-level indent-increment)) (newline o))
  (inside)
  (when when?  (set! *indent-level (- *indent-level indent-increment)) (newline o))
  (write-bytes close o))

(define (newline o)
  (write-bytes #"\n" o)
  ;; 32 = ASCII space
  (write-bytes (make-bytes *indent-level 32) o))

;; SYNTAX 
;; (block vars iterator when? loop-with o for-key ...)
;; create a thunk that iterates over a compound and writes it with appropriate separators 
(define-syntax-rule
  (block vars iterator when? loop-with o for-key ...)
  (lambda ()
    (define first? #t)
    (for ([vars iterator])
      (cond
        [first? (set! first? #f)]
        [else (write-bytes #"," o)
              (when when? (newline o))])
      for-key ...
      loop-with)))

;; JSexpr OuputPort #:encode [enc 'control] -> Void
(define (write-json-string str o #:encode [enc 'control])
  (define (escape m)
    (define ch (string-ref m 0))
    (define r
      (assoc ch '([#\backspace . "\\b"] [#\newline . "\\n"] [#\return . "\\r"]
                                        [#\page . "\\f"] [#\tab . "\\t"]
                                        [#\\ . "\\\\"] [#\" . "\\\""])))
    (define (u-esc n)
      (define str (number->string n 16))
      (define pad (case (string-length str)
                    [(1) "000"] [(2) "00"] [(3) "0"] [else ""]))
      (string-append "\\u" pad str))
    (if r
        (cdr r)
        (let ([n (char->integer ch)])
          (if (n . < . #x10000)
              (u-esc n)
              ;; use the (utf-16 surrogate pair) double \u-encoding
              (let ([n (- n #x10000)])
                (string-append (u-esc (+ #xD800 (arithmetic-shift n -10)))
                               (u-esc (+ #xDC00 (bitwise-and n #x3FF)))))))))

  ;; doesn't the library deal with this already? 
  (define rx-to-encode
    (case enc
      ;; FIXME: This should also encode (always) anything that is represented
      ;; with a \U in Racket (since the json thing should be two \u sequences,
      ;; so there should never be a \U in the output of this function); but I
      ;; don't know if there's a known specification to what gets a \U
      [(control) #rx"[\0-\37\\\"\177]"]
      [(all)     #rx"[\0-\37\\\"\177-\U10FFFF]"]
      [else (raise-type-error 'write-json-to-string "encoding symbol" enc)]))
  (write-bytes #"\"" o)
  (write-string (regexp-replace* rx-to-encode str escape) o)
  (write-bytes #"\"" o))

;; Any -> Boolean : a proper number 
(define (real-real? x) ; not nan or inf
  (and (inexact-real? x) (not (member x '(+nan.0 +inf.0 -inf.0)))))

;; ---------------------------------------------------------------------------------------------------
;; not a test

#;
(module+ test
  (define j1
    '[ ["hello"
        ["world" "good"]
        "bye"
        [[0]
         ["pretty" "dump"]
         "dump"
         "dump"
         "dump"]]
       #hasheq[(a . 0)
               (b . 1)
               (c . 2)
               (d . 3)
               (e . 4)
               (f . 5)
               (g . 6)
               (h . 7)
               (i . 8)]])
  
  (check-equal? 
   (with-input-from-string
    (with-output-to-string
     (lambda ()
       (write-json/pretty
        j1
        #:indent-lists? #t)))
    read-json)
   j1))