(import (micascheme) (asm-3 sized) (asm-2 relocable) (asm-3 sized-relocable))

(check
  (equal?
    (sized-map
      (lambda ($sized-relocable)
        (relocable-ref $sized-relocable 100))
      (sized-relocable-append
        (sized 2 (relocable-with ($org) $org))
        (sized 4 (relocable-with ($org) $org))
        (sized 5 (relocable-with ($org) $org))))
    (sized 11 '(100 102 106))))
