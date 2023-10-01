(library (tico symbolizer)
  (export
    symbolize
    symbolized-value
  (import (micascheme))

  (define symbol-environment
    (copy-environment (scheme-environment)))

  (define (symbolize $value)
    (lets
      ($symbol (generate-symbol))
      (define-top-level-value $symbol $value symbol-environment)
      $symbol))

  (define (symbolized-value $symbol)
    (top-level-value $symbol symbol-environment))
)
