(library (zx-next nextreg)
  (export
    nextreg-load
    nextreg-save)
  (import (zx-next core))

  ; preserves regs
  (define-proc (nextreg-load a)
    (preserve (bc)
      (output (a value))
      (ld bc #x243b)
      (out (c) a)
      (inc b)
      (in a (c)))
    (ret))

  ; preserves regs
  (define-proc (nextreg-save a e)
    (preserve (bc)
      (ld bc #x243b)
      (out (c) a)
      (inc b)
      (ld a e)
      (out (c) a))
    (ret))
)
