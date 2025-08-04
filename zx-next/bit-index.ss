(library (zx-next bit-index)
  (export
    load-bit-index
    load-bit-index-tc)
  (import (zx-next core))

  (define-proc (load-bit-index hl e)
    (input (hl table address) (e table index))
    (output (hl item address) (e item mask))

    ; Calculate address
    (ld a e)
    (dup 3 (rrca))
    (and #x1f)
    (add hl a)

    ; Calculate mask
    (ld a e)
    (and #x07)
    (ld b a)
    (ld e 1)
    (bsla de b)

    (ret))
)
