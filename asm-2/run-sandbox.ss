(import (asm-2 lang) (asm-2 z80) (asm-2 run))

(run
  (label loop)
  (db 243 6 0 62 2 211 254 6 0 0 0 0 0 16 250 62 5 211 254 6 0 0 0 0 0 16 250)
  (jp loop))
