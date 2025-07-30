(library (zx-next tile coord)
  (export
    tile-coord-inc
    tile-coord-index)
  (import
    (zx-next core)
    (zx-next tile map))

  (define-fragment tile-coord-inc
    (input (hl row col) (de height width))
    (output (hl row col))

    ; increment column
    (ld a l)
    (inc a)
    (cp e)
    (when z (xor a))
    (ld l a)
    (ret nz)

    ; increment row
    (ld a h)
    (inc a)
    (cp d)
    (when z (xor a))
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
