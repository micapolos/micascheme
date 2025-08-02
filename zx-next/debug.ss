(library (zx-next debug)
  (export loop-bars wait-space dump)
  (import (zx-next core) (zx-next write))

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

  (define-op (dump start size)
    (ld hl start)
    (ld bc size)
    (call write-mem))
)
