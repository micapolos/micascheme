(library (zx-next debug)
  (export
    loop-bars
    wait-space
    wait-a-sec
    dump

    press-space-string
    press-space
    press-space-tc
    press-space-proc)
  (import (zx-next core) (zx-next write))

  (define-fragment press-space-string (dz "\x10;\x6;Press \x10;\x2;SPACE\x10;\x6; to continue...\x10;\x7;"))

  (define-fragment loop-bars
    (ld a #b010)
    (ld b 0)
    (loop
      (out (#xfe) a)
      (xor #b111)
      (loop-djnz
        (dup 4 (nop)))))

  (define-fragment wait-space
    (ld bc #x7ffe)
    (loop
      (in a (c))
      (and #b00000001)
      (while z))
    (loop
      (in a (c))
      (and #b00000001)
      (while nz))
    (ret))

  (define-fragment wait-a-sec
    (ld l 10)
    (loop
      (ld h 0)
      (loop
        (ld b 0)
        (loop
          (dec b)
          (while nz))
        (dec h)
        (while nz))
      (dec l)
      (while nz))
    (ret))

  (define-ops
    ((dump start)
      (dump start #x0100))
    ((dump start size)
      (ld hl start)
      (ld bc size)
      (call write-mem)))

  (define-proc (press-space)
    (ld hl press-space-string)
    (call writeln-string)
    (jp wait-space))
)
