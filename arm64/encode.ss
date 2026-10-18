(library (arm64 encode)
  (export size-64?)
  (import (scheme))

  (define (size-64? x)
    (not (zero? (bitwise-and x #xffffffff00000000))))
)
