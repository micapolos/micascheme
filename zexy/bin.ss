(library (zexy bin)
  (export ior shl shr)
  (import (micascheme))

  (define-syntax ior (identifier-syntax bitwise-ior))
  (define-syntax shl (identifier-syntax bitwise-arithmetic-shift-left))
  (define-syntax shr (identifier-syntax bitwise-arithmetic-shift-right))
)
