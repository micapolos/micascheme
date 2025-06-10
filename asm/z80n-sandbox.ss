(import (asm z80n))

(di)
(ld a 0)
(ld b 0)
(loop-jp
  (out (#xfe) a)
  (loop-djnz (dup 5 (nop)))
  (inc a)
  (and #b111))

(run)
