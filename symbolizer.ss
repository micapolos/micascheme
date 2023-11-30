(library (symbolizer)
  (export
    symbolize
    symbolized-bound?
    symbolized-value
    symbolized-eval)
  (import (micascheme))

  (define symbolize-environment
    (copy-environment (scheme-environment)))

  (define (symbolize $value)
    (lets
      ($symbol (generate-symbol))
      (run (define-top-level-value $symbol $value symbolize-environment))
      $symbol))

  (define (symbolized-bound? $symbol)
    (top-level-bound? $symbol symbolize-environment))

  (define (symbolized-value $symbol)
    (top-level-value $symbol symbolize-environment))

  (define (symbolized-eval $datum)
    (eval $datum symbolize-environment))
)
