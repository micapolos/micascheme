(library (asm sandbox-bars)
  (export bars)
  (import (asm lang) (asm z80) (asm z80-blocks))

  (define-asm bars
    (preserve (af bc de hl)
      (ld a #b010)
      (ld b 0)
      (loop
        (out (#xfe) a)
        (xor #b111)
        (loop-djnz (nop) (nop) (nop) (nop)))))
)
