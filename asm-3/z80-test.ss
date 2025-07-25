(import (asm-3 lang) (asm-3 z80))

(check-asm
  (org #xc000)
  (ld a 10)
  (ld (hl) 12)
  (ret)
  (asm
    (start #xc000)
    (db 62 10 54 12 201)))
