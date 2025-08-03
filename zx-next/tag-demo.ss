(import (zx-next demo) (zx-next tag))

(demo
  (ld a #b00000000)
  (call write-tag)
  (call write-newline)

  (ld a #b10100000)
  (call write-tag)
  (call write-newline)

  (ld a #b11100000)
  (call write-tag)
  (call write-newline))
