(import (asm-3 base) (asm-3 sized) (asm-2 relocable) (asm-3 sized-relocable))

(check-sized-relocable 100
  (sized 3 (relocable-with ($org) (+ $org 10)))
  (sized 3 110))

(check-sized-relocable 100
  (sized-relocable-append list
    (sized 3 (org-relocable))
    (sized 5 (org-relocable))
    (sized 0 (org-relocable)))
  (sized 8 (list 100 103 108)))
