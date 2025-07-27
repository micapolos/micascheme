(import
  (asm base)
  (asm expression)
  (asm aligned)
  (asm sized)
  (asm fragment)
  (asm dependent)
  (asm relocable))

(check-fragment
  (dependent-with (foo bar) (aligned 2 (sized 4 #'relocable-binary)))
  (dependent (foo bar) (aligned 2 (sized 4 relocable-binary))))
