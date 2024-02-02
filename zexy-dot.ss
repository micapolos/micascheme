(import (zexy))

(compile-zexy "~/nextsync/dot/zexy"
  (ld a #\z)
  (rst #x10)

  (ld a #\e)
  (rst #x10)

  (ld a #\x)
  (rst #x10)

  (ld a #\y)
  (rst #x10)

  (ld a #\return)
  (rst #x10)

  (ld a #\>)
  (rst #x10)

  (ld a #\return)
  (rst #x10)

  (ret))
