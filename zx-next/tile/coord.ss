(library (zx-next tile coord)
  (export tile-coord)
  (import (zx-next core))

  (define-ops
    ((tile-coord) (tile-coord 0 0))
    ((tile-coord row col) (db row col)))
)
