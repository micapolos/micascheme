(import (zx-next test) (zx-next tagged))

(test
  (case tag-l
    (ld a #b10100000)
    (ld l #b00110)
    (tag l)
    (assert a #b10100110))

  (case tag-hl
    (ld a #b10100000)
    (ld de #x1234)
    (tag hl de)
    (assert hl #xb234))

  (case untag-l
    (ld a #b10100110)
    (untag l)
    (assert a #b10100000)
    (assert l #b00000110))

  (case untag-hl
    (ld hl #xb234)
    (untag de hl)
    (assert a #b10100000)
    (assert de #x1234)))
