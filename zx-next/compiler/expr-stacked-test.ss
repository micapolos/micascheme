(import (zx-next compiler expr-stacked))

(check-expr->stacked
  (1 (add l) (1 (ld a #x12)) (1 (ld a #x34)))
  (
    (1 (ld a #x34))
    (1 (ld a #x12))
    (1 1 1 (add l))))
