(library (zx-next bump-alloc)
  (export bump-alloc)
  (import (zx-next core))

  (define-values
    (tag-mask #b11100000)
    (size-mask #b00011111))

  (define-asm bump-alloc
    (input
      (hl - bump pointer)
      (bc - 13 bit size)
      (de - tag in bits 7 ... 5 / slot in bits 7 ... 5))
    (output
      (cf 0 = ok / 1 = out of memory)
      (hl advanced bump pointer / preserved on out of memory)
      (de allocated pointer if ok))

    ; Check out of memory by comparing bump pointer bits 15 ... 13 with slot bits.
    (ld a h)
    (and tag-mask)
    (cp e)
    (when nz (scf) (ret))

    ; Increment bump pointer to point to the last allocation byte
    (preserve (hl)
      (inc hl)
      (add hl bc)
      (ld a h)

      ; Check overflow
      (and tag-mask)
      (cp e))

    (when nz (scf) (ret))

    ; Write size with tag
    (ld (hl) c)
    (inc hl)
    (ld a b)
    (or d)
    (ld (hl) a)
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
