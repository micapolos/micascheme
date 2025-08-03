(import (zx-next demo) (zx-next tagged))

(demo
  (ld a #b10100000)
  (ld l #b00110)
  (tag l)
  (call write-tagged-byte)
  (call write-newline)

  (ld a #b10100000)
  (ld de #x1234)
  (tag hl de)
  (call write-tagged-word)
  (call write-newline)

  (ld a #b10100110)
  (untag l)
  (preserve (hl) (call write-tag))
  (writeln l)

  (ld hl #xb234)
  (untag de hl)
  (preserve (de) (call write-tag))
  (writeln de))
