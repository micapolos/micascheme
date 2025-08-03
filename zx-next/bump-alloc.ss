(library (zx-next bump-alloc)
  (export bump-alloc)
  (import (zx-next core))

  (define-asm bump-alloc
    (input
      (hl bump pointer)
      (bc size)
      (e msb-mask
        (#b1110000 to allocate within 8K bank)
        (#b1100000 to allocate within 16K segment)
        (#b0000000 to allocate without limits)))
    (output
      (cf 0 ok / 1 out of memory)
      (hl advanced bump pointer)
      (de allocated pointer))

    ; d = preserve pointer MSB
    (ld a h)
    (and e)
    (ld d a)

    ; Increment bump pointer to store allocation size
    ; de = potential allocated pointer
    (preserve (hl)
      (inc hl)
      (inc hl)

      ; Increment bump pointer to point to the last allocation byte
      (add hl bc)
      (dec hl)

      ; Check if last byte is still in the same region.
      (ld a h)
      (and e)
      (cp d))

    ; If not, return carry to indicate overflow
    (when nz
      (scf)
      (ret))

    ; Write allocated size
    (ld (hl) c)
    (inc hl)
    (ld (hl) b)
    (inc hl)

    ; DE = allocated pointer
    (ld d h)
    (ld e l)

    ; Increment bump pointer
    (add hl bc)

    ; Reset carry on success
    (rcf)
    (ret))
)
