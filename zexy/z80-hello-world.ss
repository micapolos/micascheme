(import (check) (micascheme) (zexy z80) (zexy asm))

(lets
  ($z80 (make-z80))
  ($mem (make-bytevector #x10000))
  ($rd (partial bytevector-u8-ref $mem))
  ($wr (partial bytevector-u8-set! $mem))
  ($in (lambda ($addr) 0))
  ($out
    (lambda ($addr $byte)
      (case $addr
        ((#x113B) (display (integer->char $byte)))
        (else (void)))))

  (run
    (assemble $mem
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

    (z80-run $z80 $rd $wr $in $out))
  )
