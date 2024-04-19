(import (micascheme) (asm z80-asm) (asm z80-gen))

(z80-op
  (nop)
  (ld a b)
  (ld (hl) c))
