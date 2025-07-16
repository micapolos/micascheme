(import (micascheme) (asm-2 relocable))

(check
  (equal?
    (relocable-ref
      (relocable-with 10)
      100)
    10))

(check
  (equal?
    (relocable-ref
      (relocable-with ($org) (+ $org 10))
      100)
    110))

(check
  (equal?
    (relocable-ref
      (relocable-map
        (lambda ($value) (+ $value 1))
        (relocable-with ($org) (+ $org 10)))
      100)
    111))
