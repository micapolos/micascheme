(library (zx-next bump-alloc)
  (export)
  (import (zx-next core))

  ; Allocation happens in slot 7.

  (define-op (bump-alloc)
    bump-pointer (dw #xe000))

  (define-fragment bump-allocator-alloc
    (input
      (hl bump-allocator)
      (bc 13 bit size)
      (de - tag in bits 7 ... 5 / slot in bits 7 ... 5))
    (output
      (cf 0 ok / 1 overflow)
      (de allocated-address))

    ; HL = bump pointer
    (ld e (hl))
    (inc hl)
    (ld d (hl))
    (ex de hl)

    (preserve (de)
      ; HL = bump pointer
      ; BC = size
      ; D = tag
      (ld d a)
      (call bump-alloc)

      ; HL = allocated pointer
      (ex de hl))

    ; Write back bump pointer
    (ex de hl)
    (ld (hl) d)
    (dec hl)
    (ld (hl) e)

    (ret))
)
