(import
  (asm-3 base)
  (asm-3 expression)
  (asm-2 aligned)
  (asm-3 sized)
  (asm lookable)
  (asm-3 fragment)
  (asm-3 dependent)
  (asm-2 relocable))

(check (fragment? (dependent-with () (aligned 1 (sized 2 #'foo)))))

(check-fragment
  (dependent-with (foo bar) (aligned 2 (sized 4 #'relocable-binary)))
  (dependent (foo bar) (aligned 2 (sized 4 relocable-binary))))
