(library (u8)
  (export fx->u8 u8+ u8-)
  (import (micascheme))

  (define (fx->u8 $fx)
    (fxand $fx #xff))

  (define (u8+ $a $b)
    (fx->u8 (fx+ $a $b)))

  (define (u8- $a $b)
    (fx->u8 (fx- $a $b)))
)
