(library (zexy-2 value)
  (export
    u8?
    u16?
    u2-u3-u3->u8)
  (import (micascheme))

  (define (u8? $value)
    (and (integer? $value) (>= $value 0) (< $value #xff)))

  (define (u16? $value)
    (and (integer? $value) (>= $value 0) (< $value #xffff)))

  (define (u2-u3-u3->u8 $a $b $c)
    (fxior
      (fxarithmetic-shift-left $a 6)
      (fxarithmetic-shift-left $b 3)
      $c))
)
