(library (zx-next mem)
  (export mem-fill mem-clear)
  (import (zx-next core))

  (define-fragment mem-clear
    (ld a 0)
    (jp mem-fill))

  (define-fragment mem-fill
    (input (a value) (de address) (bc size))
    (ld h d)
    (ld l e)
    (ld (de) a)
    (inc de)
    (dec bc)
    (ldir)
    (ret))
)
