(import (asm z80n))

(loop-jp
  (di)
  (ld b 0)
  (ld a #b00000010)
  (out (#xfe) a)
  (ld b 0)
  (loop-djnz (nop) (nop) (nop) (nop))
  (ld a #b00000101)
  (out (#xfe) a)
  (ld b 0)
  (loop-djnz (nop) (nop) (nop) (nop)))

(run)
