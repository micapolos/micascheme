(library (void)
  (export void?)
  (import (scheme))

  (define (void? $obj)
    (eq? $obj (void)))
)
