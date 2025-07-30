(library (zx-next scheme value)
  (export
    value-ld
    value-ref
    value-push
    value-pop
    value-byte
    value-word
    value-char
    value-false
    value-true
    value-null
    value-load-byte
    value-load-word
    value-load-char)
  (import
    (zx-next core)
    (zx-next mmu))

  (define-values
    (value-byte-tag  #b0000)
    (value-word-tag  #b0100)
    (value-char-tag  #b1010)
    (value-false-tag #b0110)
    (value-true-tag  #b1110)
    (value-null-tag  #b0010)
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

  (define-ops
    ((value-byte n)
      (ld a n)
      (ld l value-byte-tag))
    ((value-word nm)
      (ld a (fxand #xff nm))
      (ld hl (fxior (fxand #xff00 nm) #b100)))
    ((value-char n)
      (ld a n)
      (ld l value-char-tag))
    ((value-false)
      (ld l value-false-tag))
    ((value-true)
      (ld l value-true-tag))
    ((value-null)
      (ld l value-null-tag))
    ((value-load-byte)
      (ld l value-byte-tag))
    ((value-load-word)
      (ld a l)
      (ld l value-word-tag))
    ((value-load-char)
      (ld l value-char-tag)))
)
