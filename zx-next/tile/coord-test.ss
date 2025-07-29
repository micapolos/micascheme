(import (zx-next core) (zx-next tile coord) (zx-next tile map) (zx-next debug))

(define-fragments
  (top-left-coord
    (tile-coord))
  (top-right-coord
    (tile-coord 0 (- tile-map-width 1)))
  (bottom-right-coord
    (tile-coord (- tile-map-height 1) (- tile-map-width 1))))

(run
  (ld hl top-left-coord)
  (break)
  (call tile-coord-advance)
  (break)

  (ld hl top-right-coord)
  (break)
  (call tile-coord-advance)
  (break)

  (ld hl bottom-right-coord)
  (break)
  (call tile-coord-advance)
  (break)

  (call loop-bars))
