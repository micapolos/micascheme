(library (zx-next bump-allocator)
  (export)
  (import (zx-next core))

  (define-op (bump-allocator)
    bump-pointer (dw 0))

  (define-fragment bump-allocator-alloc
    (input (hl bump-allocator) (bc size) (a tag))
    (output (cf 0 ok / 1 overflow) (de allocated-address))

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
