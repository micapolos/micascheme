(import (zexy))

(compile-zexy "~/nextsync/dot/zexy"
  (org #x2000)

  (di)

  (val port-ula #xfe)

  (ld hl #xffff)
  (ld d #b00000010)

loop-hl
  (ld b h)
  (inc b)

loop-b
  (djnz loop-b)

  ; emit border and sound
  (ld a d)
  (out (#xfe) a)
  (xor #b00011111)
  (ld d a)

  (dec hl)
  (ld a h)
  (or l)
  (jp nz loop-hl)

  (ei)

  (ret))
