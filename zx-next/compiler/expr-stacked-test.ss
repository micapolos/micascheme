(import (zx-next compiler expr-stacked))

(check-expr->stacked
  (1 #t (ld a #x12))
  (
    (1 #t (ld a 18))))

(check-expr->stacked
  (1 #t (add l)
    (1 #t (ld a #x12))
    (1 #t (ld a #x13)))
  (
    (1 #t (ld a #x13))
    (1 #t (ld a #X12))
    (1 1 1 #t (add l))))

(check-expr->stacked
  (0 #f (call write-char)
    (1 #t (add l)
      (1 #t (ld a #x12))
      (1 #t (ld a #x13))))
  (
    (1 #t (ld a #x13))
    (1 #t (ld a #x12))
    (1 1 1 #t (add l))
    (1 0 #f (call write-char))))
