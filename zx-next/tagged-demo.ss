(import (zx-next demo) (zx-next tagged))

(demo
  (ld a #b10100110)
  (call write-tagged-byte)
  (call write-newline)

  (ld hl #b1010000110000000)
  (call write-tagged-word)
  (call write-newline))
