#lang racket

(define T# 3) ;; how many times each tournament is run

(provide main)

;; ---------------------------------------------------------------------------------------------------

(require (submod "admin/manager.rkt" examples)
         racket/runtime-path
         "../util/deps/swdev/Contracts/unique.rkt"
         "../util/deps/swdev/Testing/testing.rkt"
         "../util/deps/swdev/Testing/communication.rkt"
         "../util/measure.rkt"
         "admin/manager.rkt"
         "admin/referee.rkt"
         "common/basic-constants.rkt"
         "common/map.rkt"
         "common/map-serialize.rkt"
         "common/player-interface.rkt"
         "common/referee-interface.rkt"
         "player/istrategy.rkt"
         "player/player.rkt")

;; ---------------------------------------------------------------------------------------------------

(define ILL "ill-formed JSON")
(define INV "invalid JSON: ")

;; read a JSON value from current input port, validate and map to internal data ~~ or (error tag msg)
(define (get validator tag msg)
  (define x (read-message))
  (cond
    [(eof-object? x) (error tag "missing JSON")]
    [(and (string? x) (regexp-match #px"ERROR" x)) (error tag "~a:\n ~a" ILL x)])
  (define pieces (call-with-values (λ () (validator x)) list))
  (unless (first pieces) (error tag "~a ~a: ~e" INV msg x))
  (apply values pieces))

;; ---------------------------------------------------------------------------------------------------

(define-modifiable
  #:level base
  (define done (λ () (for ([_ 16176]) (log-info "heisenberg"))))

  #:level noop
  (define done void)

  #:level check
  (define done void))

(define (main big-map)
  (for ([test (in-list TESTS)])
    (match-define `[,players ,ranks ,result] test)
    (define cards (make-list CARDS-PER-GAME "white"))
    (r-check-equal? do-test `[,big-map ,players ,cards] ranks result))
  (done))

(define (do-test)
  (define gm (get (compose (game-map-validate 'xmanager) parse-game-map) 'xmanager "Map"))
  (define ps (get (get-players gm MIN-PLAYER-PER-GAME +inf.0) 'xmanager "JSON Players"))
  (define cs (get get-colors 'xmanager "JSON Colors"))

  (for ([i (in-range T#)])
    (let ([ps (map (λ (p) (p)) ps)])
      (manager ps #:cards cs)))

  (match (manager (map (λ (p) (p)) ps) #:cards cs)
    [(? string? em) (send-message em)]
    [results (send-message (manager-results->names results))]))

;; ---------------------------------------------------------------------------------------------------
(define ((game-map-validate tag) gm)
  (cond
    [(boolean? gm) #false]
    [else
     ;; if it is a game-map, the parser guarantees that there are no duplicate names and places
     ;; let's enforce size constraiints here
     (unless (<= (length (game-map-cities gm)) CITY#) (error tag "too many cities"))
     (unless (<= (set-count (game-map-all-connections gm)) CONN#) (error tag "too many connections"))
     gm]))

;; ---------------------------------------------------------------------------------------------------
(define ((get-players gm min-p max-p) j)
  (match j
    [(and `([,(? player-name? p-name) ,(? strategy-name? p-strat)] ...)
          (? (λ (j) (distinct? (map first j))))
          (? (λ (j) (<= min-p (length j) max-p))))
     (for/list ([name p-name] [strat p-strat])
       (λ ()
         (make-player-from-strategy-path (->strat strat) #:gm gm #:name name)))]
    [_ #false]))

(define (player-name? x)
  (and (string? x) (<= (string-length x) MAX-PLAYER-NAME) (regexp-match PLAYER-NAME-PX x)))

(define-runtime-paths (hold-10.rkt buy-now.rkt cheat.rkt)
  (values "player/hold-10-strategy.rkt"
          "player/buy-now-strategy.rkt"
          "player/cheat-strategy.rkt"))

(define STRATS
  `([,HOLD-10 ,hold-10.rkt]
    [,BUY-NOW ,buy-now.rkt]
    [,CHEAT   ,cheat.rkt]))

(define (strategy-name? x)
  (assoc x STRATS))

(define (->strat x)
  (second (assoc x STRATS)))

;; ---------------------------------------------------------------------------------------------------
(define (get-colors j)
  (match j
    [(and `[,(? color?) ...] (? (λ _ (= (length j) CARDS-PER-GAME)))) (map string->symbol j)]
    [_ #false]))

;; ---------------------------------------------------------------------------------------------------

(define 5players
  `[["Matthias" ,CHEAT]
    ["Alan"     ,CHEAT]
    ["Jason"    ,HOLD-10]
    ["Cameron"  ,HOLD-10]
    ["BenL"     ,BUY-NOW]])

(define mf-wins-2 (append (rest 5players) `[["Matthias" ,BUY-NOW]]))

(define (mk-holds l) (map (λ (x) (list (~a x) HOLD-10)) l))
(define 9more (mk-holds '[Cam Eshi Neha Darp Man Lana Sati Shob Sinr]))
(define yet   (mk-holds '[BenG Susi Juli Kani Petr Mowi Dhav Evan Akan Riti Deep Moha Feli]))

(define 14players (append 9more 5players))
(define 27players (append yet 14players))

(define rankings
  '[[#; "first place:" "BenL"]
    [#; "cheats:"      "Alan" "Matthias"]])

(define mf-rankings
  '[[#; "first place:" "BenL" "Matthias"]
    [#; "cheats:"      "Alan"]])

(define TESTS
  `([,5players {,rankings} "5"]
    [,mf-wins-2 {,mf-rankings} "2"]
    [,14players {,rankings} "14"]
    [,27players {,rankings} "14"]))

;; ---------------------------------------------------------------------------------------------------

(define-main-module
  #:entry-point main
  #:data "data/big-map.rktd")
