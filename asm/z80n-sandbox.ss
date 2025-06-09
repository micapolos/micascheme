(import (asm z80n))

(loop
  (ld a #b00000010)
  (out (#xfe) a)
  (ld a #b00010101)
  (out (#xfe) a))

(run)
