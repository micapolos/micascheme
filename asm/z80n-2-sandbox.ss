(import (asm z80n-2))

(run-asm
  (loop-jp
    (di)
    (ld b 0)
    (ld a #b00000010)
    (out (#xfe) a)
    (ld b 0)
    (loop-djnz (dup 4 (nop)))
    (ld a #b00000101)
    (out (#xfe) a)
    (ld b 0)
    (loop-djnz (dup 4 (nop)))))
