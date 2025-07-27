(import
  (asm-3 base)
  (asm-3 expression)
  (asm-3 aligned)
  (asm-3 sized)
  (asm-3 fragment)
  (asm-3 dependent)
  (asm-3 relocable))

(check-fragment
  (dependent-with (foo bar) (aligned 2 (sized 4 #'relocable-binary)))
  (dependent (foo bar) (aligned 2 (sized 4 relocable-binary))))
