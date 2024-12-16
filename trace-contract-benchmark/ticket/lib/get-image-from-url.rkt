#lang racket/gui

;; retrieve image via a relative file path, a file URL, or a net URL

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
 (contract-out
  [png-from-url (-> string? image?)]))

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

(require net/url)

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

#; {String -> Image}
(define (png-from-url url-as-string)
  (define url (string->url url-as-string))
  (cond
    [(boolean? (url-scheme url))
     (unless (file-exists? url-as-string) (error 'png-from-url "expected path to file, given ~e" url))
     (define bm (make-object bitmap% url-as-string 'png #false #true 1.0 #true))
     (unless (send bm ok?) (error 'png-from-url "something went wrong with the bitmap creation"))
     bm]
    [else 
     (call/input-url url
                     (λ (url) (get-pure-port url #:redirections 20))
                     (λ (port) (make-object bitmap% port 'unknown #f)))]))

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

  (define net-url  "https://www.felleisen.org/Images/christopher.png")
  (define file-url "file:/Users/matthias/Courses/21SwDev/Source/Images/map.png")
  (define rel-path "../../../Courses/21SwDev/Source/Images/map.png")

  (define net-pict  (png-from-url net-url))
  (define file-pict (png-from-url file-url))
  (define rel-pict  (png-from-url rel-path))

  rel-pict)