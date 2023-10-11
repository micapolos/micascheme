(import (micascheme) (fstack))

(define-ftype vec
  (struct
    (x float)
    (y float)))

(define (vec-x $vec) (ftype-ref vec (x) $vec))
(define (vec-y $vec) (ftype-ref vec (y) $vec))

(define (vec-set $vec $x $y)
  (ftype-set! vec (x) $vec $x)
  (ftype-set! vec (y) $vec $y))

(define (vec-copy $vec $vec2)
  (vec-set $vec (vec-x $vec2) (vec-y $vec2)))

(define (vec-length $vec)
  (lets
    ($x (vec-x $vec))
    ($y (vec-y $vec))
    (sqrt (+ (* $x $x) (* $y $y)))))

(define (vec-multiply $vec $flonum)
  (vec-set $vec
    (* (vec-x $vec) $flonum)
    (* (vec-y $vec) $flonum)))

(check
  (equal?
    (fstack-run 16
      (fstack-local vec $vec)
      (fstack-block
        (fstack-local vec $vec2)
        (vec-set $vec2 3.0 4.0)
        (vec-multiply $vec2 2)
        (vec-copy $vec $vec2))
      (vec-length $vec))
    10.0))
