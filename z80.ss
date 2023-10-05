(library (z80)
  (export z80)
  (import (micascheme) (z80-parser))

  (define-syntax-rule (z80 $item ...)
    (syntax->bytevector #`($item ...)))
)
