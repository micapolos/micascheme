(library (zx-next tile coord)
  (export
    tile-coord-advance
    tile-coord-index)
  (import
    (zx-next core)
    (zx-next tile map))

  ; Advances tile coord by 1 character, wrapping to the next row if needed
  ; reporting carry if last row is reached, and scroll-up is necessary.
  (define-fragment tile-coord-advance
    (input (hl row col) (de height width))
    (output (hl row col) (cf scroll-up))

    ; increment column
    (ld a l)
    (inc a)
    (cp e)
    (if p
      (then (xor a) (scf))
      (else (rcf)))
    (ld l a)
    (ret nc)

    ; increment row
    (ld a h)
    (inc a)
    (cp d)
    (if p
      (then (dec a) (scf))
      (else (rcf)))
    (ld h a)
    (ret))

  (define-fragment tile-coord-index
    (input (hl row col) (de height width))
    (output (hl index))
    (ld d h)
    (mul d e)
    (ld h 0)
    (ex de hl)
    (add hl de)
    (ret))
)
