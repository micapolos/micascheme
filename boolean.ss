(library (boolean)
  (export false? true?)
  (import (scheme))

  (define (false? $value) (not $value))
  (define (true? $value) (not (false? $value)))
)
