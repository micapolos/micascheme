(proc debug-bars
  (ld a #b010)
  (ld b 0)
  (loop
    (out (#xfe) a)
    (xor #b111)
    (loop-djnz (nop) (nop) (nop) (nop))))
