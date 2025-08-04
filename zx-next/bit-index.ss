(library (zx-next bit-index)
  (export lbi bii bid)
  (import (zx-next core))

  (define-ops (keywords de b)
    ; Load bit index
    ; d = (b >> 3) & 1f
    ; e = 1 << (b & 7)
    ; af, bc = corrupted
    ((lbi de b)
      ; Extract address
      (ld a b)
      (dup 3 (rrca))
      (and #x1f)
      (ld c a)

      ; Calculate mask
      (ld a b)
      (and #x07)
      (ld b a)
      (ld de 1)
      (ld d c)
      (bsla de b))

    ; Load bit index with constant
    ; d = (n >> 3) & 1f
    ; e = 1 << (n & 7)
    ; af, bc = corrupted
    ((lbi de n)
      (ld b n)
      (lbi de b))

    ; Increment bit index
    ; e = rotate left
    ; d = increment on carry from e
    ; af = corrupted
    ((bii de)
      (ld a e)
      (rlca)
      (ld e a)
      (when c (inc d)))

    ; Decrement bit index
    ; e = rotate right
    ; d = decrement on carry from e
    ; af = corrupted
    ((bid de)
      (ld a e)
      (rrca)
      (ld e a)
      (when c (dec d))))
)
