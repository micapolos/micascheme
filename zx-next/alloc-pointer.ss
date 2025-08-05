(library (zx-next alloc-pointer)
  (export
    alloc-pointer-init
    alloc-pointer-init-tc
    alloc-pointer-alloc
    alloc-pointer-alloc-tc)
  (import
    (zx-next core)
    (zx-next tag))

  ; Bump pointer is used for allocation within a 8K bank in dedicated slot.
  ; It's a 13-bit address tagged with slot number.
  ; Address #x1fff means bank is full.
  ; Allocation starts with tagged size followed by allocated region of memory.
  ; Maximum allocable region is #1ffe.

  (define-proc (alloc-pointer-init hl)
    (inc hl)
    (ld (hl) 0)
    (ret))

  (define-proc (alloc-pointer-alloc hl bc)
    (input
      (hl - bump pointer)
      (bc - tagged size))
    (output
      (cf - 0 = ok / 1 = out of memory)
      (hl - advanced bump pointer / preserved if out of memory)
      (de - allocated pointer if ok))

    ; Check out of memory
    (ld a h)
    (and tag-inv-mask)
    (cp tag-inv-mask)
    (when z
      (ld a l)
      (cp #xff)
      (when z (ret-c)))

    ; E - slot tag
    (ld a h)
    (and tag-mask)
    (ld e a)

    ; Increment bump pointer to check for overflow
    (preserve (bc hl)
      ; Increment by 2 bytes of tagged size
      (inc hl)
      (inc hl)

      ; Untag size
      (ld a b)
      (and tag-inv-mask)
      (ld b a)

      ; Increment by size
      (add hl bc)

      ; one more byte to trigger overflow.
      (inc hl)

      ; Check overflow
      (ld a h)
      (and tag-mask)
      (cp e))

    (when nz (ret-c))

    ; Write tagged size
    (ld (hl) c)
    (inc hl)
    (ld (hl) b)
    (inc hl)

    ; DE = allocated pointer
    (ld de hl)

    ; Untag size
    (ld a b)
    (and tag-inv-mask)
    (ld b a)

    ; Increment by size
    (add hl bc)

    ; initialize tag of next entry
    (inc hl)
    (ld (hl) 0)
    (dec hl)

    ; Return NC on success
    (ret-nc))
)
