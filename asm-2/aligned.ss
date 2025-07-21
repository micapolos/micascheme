(library (asm-2 aligned)
  (export
    aligned aligned? aligned-alignment aligned-ref
    pure-aligned
    aligned-more?
    aligned-sort
    aligned-sorted-refs
    aligned->datum
    aligned-append-map
    aligned-map
    map-aligned)
  (import (asm-3 base))

  (define-annotated (aligned alignment))

  (define (pure-aligned $obj)
    (aligned 1 $obj))

  (define (aligned-append-map $ref-append . $list)
    (aligned
      (apply max (map aligned-alignment $list))
      (apply $ref-append (map aligned-ref $list))))

  (define (aligned-more? $aligned-1 $aligned-2)
    (>
      (aligned-alignment $aligned-1)
      (aligned-alignment $aligned-2)))

  (define (aligned-sort $aligned-list)
    (sort aligned-more? $aligned-list))

  (define (aligned-sorted-refs $aligned-list)
    (map aligned-ref (aligned-sort $aligned-list)))

  (define (aligned->datum $aligned)
    `(aligned
      ,(aligned-alignment $aligned)
      ,(aligned-ref $aligned)))
)
