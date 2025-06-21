(library (asm-2 z80)
  (export ret)
  (import (asm-2 lang))

  (define-macro (ret) (db #xc9))
)
