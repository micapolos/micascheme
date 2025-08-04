(library (zx-next bump-pointer)
  (export
    bump-pointer-init
    bump-pointer-init-tc
    bump-pointer-alloc
    bump-pointer-alloc-tc)
  (import (zx-next core))

  ; Bump pointer is used for allocation within a 8K bank in dedicated slot.
  ; It consists of 13-bit address in dedicated slot.
  ; If MSB bits do not match the dedicated slot, it indicates out of memory.
  ; Allocation starts with 13-bit size with a 3-bit tag,
  ; followed by allocated region of memory.

  (define-values
    (tag-mask #b11100000)
    (size-mask #b00011111))

  (define-proc (bump-pointer-init e)
    (input (e - allocation slot in bits 7 ... 5))
    (output (hl - bump pointer))

    ; initialize pointer
    (ld h e)
    (ld l 0)

    ; load tag zero
    (inc hl)
    (ld (hl) 0)
    (dec hl)

    (ret))

  (define-proc (bump-pointer-alloc hl de bc)
    (input
      (de - non-zero tag in bits 7 ... 5 / allocation slot in bits 7 ... 5)
      (hl - bump pointer / out of memory if outside of allocation slot)
      (bc - 13 bit size))
    (output
      (cf - 0 = ok / 1 = out of memory)
      (hl - advanced bump pointer / preserved if out of memory)
      (de - allocated pointer if ok))

    ; Check out of memory by comparing bump pointer bits 15 ... 13 with slot bits.
    (ld a h)
    (and tag-mask)
    (cp e)
    (when nz (ret-c))

    ; Increment bump pointer to check for overflow
    (preserve (hl)
      ; size word
      (inc hl)
      (inc hl)

      ; allocated block
      (add hl bc)

      ; one more byte - space for the zeroed tag for the next block
      (inc hl)

      (ld a h)

      ; Check overflow
      (and tag-mask)
      (cp e))

    (when nz (ret-c))

    ; Write size with tag
    (ld (hl) c)
    (inc hl)
    (ld a b)
    (or d)
    (ld (hl) a)
    (inc hl)

    ; DE = allocated pointer
    (ld de hl)

    ; Increment bump pointer
    (add hl bc)

    ; initialize tag
    (inc hl)
    (ld (hl) 0)
    (dec hl)

    ; Reset carry on success
    (ret-nc))
)
