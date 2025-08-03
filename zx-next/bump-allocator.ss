(library (zx-next bump-allocator)
  (export
    bump-allocator-data
    bump-allocator-size
    bump-allocator-init
    bump-allocator-alloc)
  (import (zx-next core))

  ; Allocation happens in slot 7.
  ; Allocated pointers are 13-bits.

  (define-values
    (tag-mask  #b11100000)
    (size-mask #b00011111)
    (slot 7)
    (bump-allocator-size 2))

  (define-op (bump-allocator-data) (dw #xe000))

  (define-fragment bump-allocator-init
    (input (hl - bump allocator pointer))
    (ld (hl) #x00)
    (inc hl)
    (ld (hl) #xe0)
    (ret))

  (define-fragment bump-allocator-alloc
    (input
      (hl - bump-allocator pointer)
      (bc - non-zero tag in bits 15 ... 13 / size in bits 12 ... 0))
    (output
      (cf - 0 ok / 1 overflow)
      (de - allocated-address))

    (preserve (hl)
      ; hl = bump pointer
      (ld e (hl))
      (inc hl)
      (ld d (hl))
      (ex de hl)

      ; BC = size
      (ld a b)
      (and size-mask)
      (ld b a)

      ; D = tag
      (ld a b)
      (and tag-mask)
      (ld d a)

      ; E = slot
      (ld e slot)

      ; DE = allocated pointer
      ; HL = bump pointer
      (call bump-alloc)

      ; BC = bump-pointer
      (ld b h)
      (ld c l))

    ; Write back bump pointer to bump-allocator
    (ld (hl) b)
    (dec hl)
    (ld (hl) c)

    (ret))
)
