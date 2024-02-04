(library (vector)
  (export
    build-vector
    build-immutable-vector)
  (import (scheme))

  (define (build-vector $size $proc)
    (let
      (($vector (make-vector $size)))
      (do
        (($index 0 (add1 $index)))
        ((= $index $size) $vector)
        (vector-set! $vector $index ($proc $index)))))

  (define (build-immutable-vector $size $proc)
    (vector->immutable-vector (build-vector $size $proc)))
)
