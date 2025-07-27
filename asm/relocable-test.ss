(import (micascheme) (asm relocable))

(check-relocable 100
  (relocable-with 10)
  10)

(check-relocable 100
  (org-relocable)
  100)

(check-relocable 100
  (relocable-with ($org) (+ $org 10))
  110)

(check-relocable 100
  (relocable-map
    (relocable-with ($org) (+ $org 10))
    (lambda ($value) (+ $value 1)))
  111)

(check-relocable 100
  (relocable+offset (relocable-with ($org) $org) 10)
  110)

(check-relocable 100
  (relocable-append
    (relocable-with ($org) $org)
    (relocable-with ($org) (+ $org 1)))
  '(100 101))
