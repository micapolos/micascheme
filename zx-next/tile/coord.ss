(library (zx-next tile coord)
  (export
    tile-coord
    tile-coord-advance)
  (import
    (zx-next core)
    (zx-next tile map))

  (define-ops
    ((tile-coord) (tile-coord 0 0))
    ((tile-coord row col) (db col row)))

  ; Advances tile coord by 1 character, wrapping to the next row if needed
  ; reporting carry if last row is reached, and scroll-up is necessary.
  (define-fragment tile-coord-advance
    ; increment column
    (ld a (hl))
    (inc a)
    (cp tile-map-width)
    (if m
      (then (rcf))
      (else (ld a 0) (scf)))
    (ld (hl) a)
    (ret nc)

    ; increment row
    (inc hl)
    (ld a (hl))
    (inc a)
    (cp tile-map-height)
    (if m
      (then (rcf))
      (else (dec a) (scf)))
    (ld (hl) a)
    (ret))
)
