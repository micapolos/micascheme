(import (micascheme) (micac c) (check))

(check
  (equal?
    (micac-c
      (var u8 x)
      (set x 10))
    (lines-string
      "uint8_t x;"
      "x = 10;")))
