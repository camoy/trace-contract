#lang racket/gui

;; encode PNG files as data-urls, extract PNG images from data urls

;                                                          
;                                                          
;                                                          
;                                             ;            
;                                             ;            
;    ;;;;   ;   ;;  ; ;;;    ;;;;    ;;;;;  ;;;;;;   ;;;;  
;    ;  ;;   ;  ;   ;;  ;   ;;  ;;   ;;       ;     ;    ; 
;   ;    ;    ;;    ;    ;  ;    ;   ;        ;     ;      
;   ;;;;;;    ;;    ;    ;  ;    ;   ;        ;     ;;;    
;   ;         ;;    ;    ;  ;    ;   ;        ;        ;;; 
;   ;         ;;    ;    ;  ;    ;   ;        ;          ; 
;    ;       ;  ;   ;;  ;   ;;  ;;   ;        ;     ;    ; 
;    ;;;;;  ;    ;  ; ;;;    ;;;;    ;         ;;;   ;;;;  
;                   ;                                      
;                   ;                                      
;                   ;                                      
;                                                          

(require 2htdp/image)

(provide

 is-data-url?
 
 (contract-out
  [image->data-url (-> image? is-data-url?)]
  [extract-image   (-> is-data-url? image?)]
  [create-data-url (-> (or/c string? path?) bytes?)]))

;                                                                                                  
;                                                                                                  
;        ;                                       ;                             ;                   
;        ;                                       ;                             ;                   
;        ;                                       ;                                                 
;    ;;; ;   ;;;;   ; ;;;    ;;;;   ; ;;;    ;;; ;   ;;;;   ; ;;;     ;;;    ;;;     ;;;;    ;;;;  
;    ;  ;;   ;  ;;  ;;  ;    ;  ;;  ;;   ;   ;  ;;   ;  ;;  ;;   ;   ;   ;     ;     ;  ;;  ;    ; 
;   ;    ;  ;    ;  ;    ;  ;    ;  ;    ;  ;    ;  ;    ;  ;    ;  ;          ;    ;    ;  ;      
;   ;    ;  ;;;;;;  ;    ;  ;;;;;;  ;    ;  ;    ;  ;;;;;;  ;    ;  ;          ;    ;;;;;;  ;;;    
;   ;    ;  ;       ;    ;  ;       ;    ;  ;    ;  ;       ;    ;  ;          ;    ;          ;;; 
;   ;    ;  ;       ;    ;  ;       ;    ;  ;    ;  ;       ;    ;  ;          ;    ;            ; 
;    ;  ;;   ;      ;;  ;    ;      ;    ;   ;  ;;   ;      ;    ;   ;   ;     ;     ;      ;    ; 
;    ;;; ;   ;;;;;  ; ;;;    ;;;;;  ;    ;   ;;; ;   ;;;;;  ;    ;    ;;;   ;;;;;;;  ;;;;;   ;;;;  
;                   ;                                                                              
;                   ;                                                                              
;                   ;                                                                              
;                                                                                                  

(require net/base64)
#;(module+ picts
  (require (submod "..")))

;                                                                          
;                                                                          
;      ;;;                                     ;                           
;     ;                               ;        ;                           
;     ;                               ;                                    
;   ;;;;;;  ;    ;  ; ;;;     ;;;   ;;;;;;   ;;;     ;;;;   ; ;;;    ;;;;  
;     ;     ;    ;  ;;   ;   ;   ;    ;        ;    ;;  ;;  ;;   ;  ;    ; 
;     ;     ;    ;  ;    ;  ;         ;        ;    ;    ;  ;    ;  ;      
;     ;     ;    ;  ;    ;  ;         ;        ;    ;    ;  ;    ;  ;;;    
;     ;     ;    ;  ;    ;  ;         ;        ;    ;    ;  ;    ;     ;;; 
;     ;     ;    ;  ;    ;  ;         ;        ;    ;    ;  ;    ;       ; 
;     ;     ;   ;;  ;    ;   ;   ;    ;        ;    ;;  ;;  ;    ;  ;    ; 
;     ;      ;;; ;  ;    ;    ;;;      ;;;  ;;;;;;;  ;;;;   ;    ;   ;;;;  
;                                                                          
;                                                                          
;                                                                          
;                                                                          

(define data-url-header  #"data:image/png;base64,")
(define data-url-pattern (byte-pregexp (bytes-append data-url-header #"(.+)")))

(define (is-data-url? x)
  (and (bytes? x) (regexp-match data-url-pattern x) #true))

#; {DatURL -> Image}
(define (extract-image url)
  (define is-data-url (regexp-match data-url-pattern url))
  (unless is-data-url
    (error 'extract-image "expected data url, given ~e" url))
  (define png-bytes (base64-decode (second is-data-url)))
  (define obj
    (with-input-from-bytes png-bytes
      (λ () (make-object bitmap% (current-input-port) 'png #f))))
  (unless (send obj ok?)
    (error 'extract-image "expected PNG bytes, given ~e" png-bytes))
  obj)

#; {FilePath -> DataUrl}
(define (create-data-url fp)
  (unless (file-exists? fp) (error 'create-data-url "expected path to file, given ~e" fp))
  (define bm (make-object bitmap% fp 'png #false #true 1.0 #true))
  (unless (send bm ok?) (error 'create-data-url "something went wrong with the bitmap creation"))
  (define by (send bm get-data-from-file))
  (unless by (error 'create-data-url "expected bytes from ~a, something went wrong" fp))
  (define b3 (vector-ref by 2))
  (bytes-append data-url-header (base64-encode b3)))

(define (image->data-url image)
  (define filename (make-temporary-file))
  (save-image image filename)
  (create-data-url filename))



;                                          
;                                          
;              ;                           
;              ;              ;            
;                             ;            
;   ; ;;;    ;;;      ;;;   ;;;;;;   ;;;;  
;   ;;  ;      ;     ;   ;    ;     ;    ; 
;   ;    ;     ;    ;         ;     ;      
;   ;    ;     ;    ;         ;     ;;;    
;   ;    ;     ;    ;         ;        ;;; 
;   ;    ;     ;    ;         ;          ; 
;   ;;  ;      ;     ;   ;    ;     ;    ; 
;   ; ;;;   ;;;;;;;   ;;;      ;;;   ;;;;  
;   ;                                      
;   ;                                      
;   ;                                      
;                                          

#;(module+ picts

  (define file-path "../../../Courses/21SwDev/Source/Images/map.png")
  (define bar-obj (extract-image (create-data-url file-path)))

  (define stuff (image->data-url bar-obj)))
