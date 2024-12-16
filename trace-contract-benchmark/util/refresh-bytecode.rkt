#lang racket/base

(require compiler/cm
         "constants.rkt")

(module+ main
  (putenv LVL-VAR (car LEVELS))
  (parameterize ([current-namespace (make-base-namespace)])
    (define manager (make-compilation-manager-load/use-compiled-handler))
    (parameterize ([current-load/use-compiled manager])
      (for ([benchmark-path (in-list BENCHMARK-PATHS)])
        (dynamic-require benchmark-path #f)))))
