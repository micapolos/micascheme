(import (asm lang) (asm z80) (asm run) (asm asm) (asm asm-core))

(asm-run
  (di)
  (loop
    (ld b 0)
    (ld a #b00000010)
    (out (#xfe) a)
    (ld b 0)
    (loop-djnz
      (nop)
      (nop)
      (nop)
      (nop))
    (ld a #b00000101)
    (out (#xfe) a)
    (ld b 0)
    (loop-djnz
      (nop)
      (nop)
      (nop)
      (nop))))
