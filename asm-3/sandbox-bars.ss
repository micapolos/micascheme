(library (asm-3 sandbox-bars)
  (export bars)
  (import (asm-3 lang) (asm-3 z80))

  (define-asm bars
    (ld a #b010)
    (ld b 0)
    loop
    (out (#xfe) a)
    (xor #b111)
    stripe
    (nop) (nop) (nop) (nop)
    (djnz stripe)
    (jr loop))
)
