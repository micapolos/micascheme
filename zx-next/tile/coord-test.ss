(import (zx-next core) (zx-next tile coord) (zx-next debug))

(define-fragments
  (coord-1 (tile-coord))
  (coord-2 (tile-coord 10 20)))

(run (call loop-bars))
