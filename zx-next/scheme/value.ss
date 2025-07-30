(library (zx-next scheme value)
  (export
    value-ld
    value-ref
    value-push
    value-pop)
  (import
    (zx-next core)
    (zx-next mmu))

  (define-values
    (slot 7))

  ; Value in chez scheme is stored in registers DE and HL
  ; D - flags
  ;   bit 7 - pointer vs data
  ; E - high 8 bits
  ; HL - low 16 bits

  ; Dereferences pointer value, by switching to be given bank and
  ; reading new value.
  (block value-ref
    (ld a e)
    (mmu slot a)
    (ld e (hl))
    (inc hl)
    (ld d (hl))
    (inc hl)
    (ld a (hl))
    (inc hl)
    (ld h (hl))
    (ld l a)
    (ret))

  (define-ops
    ((value-ld n)
      (ld de (fxsrl n 16))
      (ld hl (fxand n #xffff)))
    ((value-push)
      (push de)
      (push hl))
    ((value-pop)
      (pop hl)
      (pop de)))
)
