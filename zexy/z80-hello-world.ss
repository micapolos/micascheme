(import (check) (micascheme) (zexy z80) (zexy asm))

(lets
  (z80 (make-z80))
  (vec (make-bytevector #x10000))
  (mem (partial bytevector-u8-ref vec))
  (mem! (partial bytevector-u8-set! vec))
  (io (lambda ($addr) 0))
  (io!
    (lambda ($addr $byte)
      (case1 $addr
        (#x113b (display (integer->char $byte)))
        ((else _) (void)))))

  (run
    (assemble vec
      (ld bc #x113b)

      (ld a #\H) (out (c) a)
      (ld a #\e) (out (c) a)
      (ld a #\l) (out (c) a)
      (ld a #\l) (out (c) a)
      (ld a #\o) (out (c) a)
      (ld a #\,) (out (c) a)

      (ld a #\space) (out (c) a)

      (ld a #\w) (out (c) a)
      (ld a #\o) (out (c) a)
      (ld a #\r) (out (c) a)
      (ld a #\l) (out (c) a)
      (ld a #\d) (out (c) a)
      (ld a #\!) (out (c) a)

      (ld a #\newline) (out (c) a)

      (halt))

    (z80-run z80 mem mem! io io!)
  )
)

