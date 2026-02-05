(library (fixnum)
  (export
    fx+1/wraparound
    fx-1/wraparound)
  (import (scheme))

  (define (fx+1/wraparound x)
    (fx+/wraparound x 1))

  (define (fx-1/wraparound x)
    (fx-/wraparound x 1))
)
