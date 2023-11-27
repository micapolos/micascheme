(library (boolean)
  (export
    false?)
  (import (scheme))

  (define (false? $value)
    (not $value))
)
