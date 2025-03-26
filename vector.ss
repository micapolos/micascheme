(library (vector)
  (export
    build-vector
    build-immutable-vector
    list->immutable-vector)
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

  (define (list->immutable-vector $list)
    (vector->immutable-vector (list->vector $list)))
)
