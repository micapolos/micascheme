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
    (fstack-run 8
      (fstack-define vec $vec)
      (vec-set $vec 3.0 4.0)
      (vec-multiply $vec 2)
      (vec-length $vec))
    10.0))
