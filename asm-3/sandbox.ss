(import (asm-3 run))

(run
  (ld a #b010)
  (ld b 0)
  loop
  (out (#xfe) a)
  (xor #b111)
  stripe
  (nop) (nop) (nop)
  (dec b)
  (jp nz stripe)
  (jp loop))
