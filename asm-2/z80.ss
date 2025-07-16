(library (asm-2 z80)
  (export nop ret)
  (import (syntaxes) (asm-2 core))

  (define-rules-syntaxes
    ((nop) (db #x00))
    ((ret) (db #xc9)))
)
