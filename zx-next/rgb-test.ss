(import (zx-next test) (zx-next rgb))

(define-fragments
  (rgb-8 (rgb-332 #b101 #b010 #b01))
  (rgb-9 (rgb-333 #b101 #b010 #b001)))

(test
  (case rgb-332
    (ld a (rgb-8))
    (assert a #b10101001)

  (case rgb-333
    (ld hl (rgb-9))
    (assert hl #b110101000))))
