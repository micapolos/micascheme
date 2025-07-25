(library (zx-next scheme value)
  (export ref)
  (import (zx-next core))

  (define slot 7)

  ; Value in chez scheme is stored in registers DE and HL
  ; D - flags
  ;   bit 7 - pointer vs data
  ; E - high 8 bits
  ; HL - low 16 bits

  ; Dereferences pointer value, by switching to be given bank and
  ; reading new value.
  (proc ref
    (ld a e)
    (nextreg (+ #x50 slot) a)
    (ld e (hl))
    (inc hl)
    (ld d (hl))
    (inc hl)
    (ld a (hl))
    (inc hl)
    (ld h (hl))
    (ld l a)
    (ret))
)
