(library (boolean)
  (export false? not-false?)
  (import (scheme))

  (define (false? $value) (not $value))
  (define (not-false? $value) (not (false? $value)))
)
