(import (zx-next test) (zx-next tagged))

(test
  (case tagged-byte
    (ld a (tagged-byte #xa0 #x13))
    (assert a #xb3))

  (case tagged-word
    (ld hl (tagged-word #xa0 #x1234))
    (assert hl #xb234))

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
    (assert de #x1234))

  (case untag-bc
    (ld bc #xb234)
    (untag de bc)
    (assert a #b10100000)
    (assert de #x1234)))
