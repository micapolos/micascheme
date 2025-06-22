(import (asm lang) (asm z80) (asm run) (asm asm) (asm asm-core))

(asm-run
  (di)
  (ld b 0)
  (loop
    (ld a #b00000010)
    (out (#xfe) a)
    (loop-djnz
      (nop)
      (nop)
      (nop)
      (nop))
    (ld a #b00000101)
    (out (#xfe) a)
    (loop-djnz
      (nop)
      (nop)
      (nop)
      (nop))))
