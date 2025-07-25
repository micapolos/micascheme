(library (asm-3 sandbox-bars)
  (export bars)
  (import (asm-3 lang) (asm-3 z80) (asm-3 z80-blocks))

  (define-asm bars
    (ld a #b010)
    (ld b 0)
    (loop
      (out (#xfe) a)
      (xor #b111)
      (loop-djnz (nop) (nop) (nop) (nop))))
)
