(library (zx-next bit-index)
  (export
    lbi bii bid
    load-bit-index
    load-bit-index-tc)
  (import (zx-next core))

  ; Bit index is 8-bit address in D and 8-bit mask in E.
  ; It can be used for lookup in bit tables.

  (define-ops (keywords de d)
    ; Load bit index from E into DE
    ; d = (e >> 3) & 1f
    ; a, e = #x80 >> (e & 7)
    ((lbi de)
      (ld a e)
      (rrca)
      (rrca)
      (rrca)
      (and #x1f)
      (ld d a)
      (setae)
      (ld e a))

    ; Load constant bit index into DE
    ; e = n
    ; d = (e >> 3) & 1f
    ; a, e = #x80 >> (e & 7)
    ((lbi d n)
      (ld e n)
      (lbi de))

    ; Increment bit index in DE
    ; a, e = rotate right
    ; d = increment on carry from e
    ((bii de)
      (ld a e)
      (rrca)
      (ld e a)
      (when c (inc d)))

    ; Decrement bit index in DE
    ; a, e = rotate left
    ; d = increment on carry from e
    ((bid de)
      (ld a e)
      (rlca)
      (ld e a)
      (when c (dec d))))

  (define-proc (load-bit-index e)
    (lbi de)
    (ret))
)
