(import (asm lang))

(loop
  (ld a #b00000010)
  (out (#xfe) a)
  (ld a #b00010101)
  (out (#xfe) a))

(start)
