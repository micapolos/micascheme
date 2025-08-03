(import (zx-next demo) (zx-next dispatch))

(define-asm writeln-number
  (input (a number))
  (and #b11)
  (ret-dispatch
    (writeln "zero")
    (writeln "one")
    (writeln "two")
    (writeln "larger than two")))

(demo
  (ld a 0)
  (call writeln-number)
  (ld a 1)
  (call writeln-number)
  (ld a 2)
  (call writeln-number)
  (ld a #xff)
  (call writeln-number))
