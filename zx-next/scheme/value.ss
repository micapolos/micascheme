(library (zx-next scheme value)
  (export
    byte-value
    word-value
    char-value

    d->value
    a->value
    value->d
    value->a
    a->char-value
    bc->value
    value->bc
    value->de
    value->tag
    value->hl
    value->mmu/hl)
  (import
    (zx-next core)
    (zx-next mmu)
    (zx-next scheme tag))

  ; Value is stored in BCDE:
  ; - B:
  ;   - bits 7..5: tag
  ;   - bits 4..0: address MSB / 21-bit value MSB / constant value
  ; - C: address LSB / 16-bit value MSB
  ; - D: 8-bit value / 16-bit value MSB / bank
  ; - E: stack offset to the previous value on the stack

  (define-values
    (mmu-slot 7)
    (addr-base-h #b11100000))

  (define-ops
    ((d->value)
      (input (a byte))
      (output (bcd value))
      (ld bc 0))

    ((a->value)
      (input (a byte))
      (output (bcd value))
      (ld d a)
      (ld bc 0))

    ((byte-value n)
      (output (bcd value))
      (ld d n)
      (ld bc #b0000000000000000))

    ((word-value nn)
      (output (bcd value))
      (ld d (fxand nn #xff))
      (ld c (fxand (fxsrl nn 8) #xff))
      (ld b #b00100000))

    ((char-value n)
      (input (a byte))
      (output (bcd value))
      (ld d n)
      (ld bc #b0100000000000000))

    ((a->char-value)
      (input (a byte))
      (output (bcd value))
      (ld d a)
      (ld bc #b0100000000000000))

    ((value->d)
      (input (bcd value))
      (output (a byte)))

    ((value->a)
      (input (bcd value))
      (output (a byte))
      (ld a d))

    ((bc->value)
      (input (bc word))
      (output (bcd value))
      (ld d c)
      (ld c b)
      (ld b word-tag))

    ((value->bc)
      (input (bc word))
      (output (bcd value))
      (ld b c)
      (ld c d))

    ((value->de)
      (input (bc word))
      (output (bcd value))
      (ld d c)
      (ld e d))

    ((value->tag)
      (input (bcc value))
      (output (a tag))
      (ld a d)
      (and tag-mask))

    ((value->hl)
      (input (bc word))
      (output (bcd value))
      (ld h c)
      (ld l d))

    ((value->mmu/hl)
      (input (bcd value))
      (output (mmu paged-in) (hl address))
      (ld a d)   ; bank in D
      (mmu mmu-slot a)
      (ld a b)   ; tag/addr in BC
      (or addr-base-h)
      (ld h a)
      (ld l c)))
)
