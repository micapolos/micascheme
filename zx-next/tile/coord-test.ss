(import (zx-next core) (zx-next tile coord) (zx-next debug))

(run
  (ld hl #x0000)
  (ld de #x1020)
  (break)
  (call tile-coord-advance)
  (break)

  (ld hl #x081f)
  (ld de #x1020)
  (break)
  (call tile-coord-advance)
  (break)

  (ld hl #x0f1f)
  (ld de #x1020)
  (break)
  (call tile-coord-advance)
  (break)

  (ld hl #x0205)
  (ld de #x1020)
  (break)
  (call tile-coord-index)
  (break)

  (call loop-bars))
