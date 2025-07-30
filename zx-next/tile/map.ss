(library (zx-next tile map)
  (export
    tile-map-width
    tile-map-height
    tile-map-scroll-up)
  (import (zx-next core))

  (define-values
    (tile-map-width 80)
    (tile-map-height 32))

  (define-fragment tile-map-scroll-up
    (ld hl (+ tile-map row-size))
    (ld de tile-map)
    (ld bc (* (- height 1) row-size))
    (ldir)
    ; clear last row
    (ld hl (+ tilemap (* row-size (- height 1))))
    (ld b width)
    (ld a (attr))
    (loop-djnz
      (ld (hl) 0)  ; space
      (inc hl)
      (ld (hl) a)
      (inc hl))
    (ret))
)
