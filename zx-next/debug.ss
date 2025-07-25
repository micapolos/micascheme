(library (zx-next debug)
  (export loop-bars)
  (import (zx-next))

  (define-asm loop-bars
    (ld a #b010)
    (ld b 0)
    (loop
      (out (#xfe) a)
      (xor #b111)
      (loop-djnz
        (dup 4 (nop)))))
)
