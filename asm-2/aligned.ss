(library (asm-2 aligned)
  (export
    aligned aligned? aligned-alignment aligned-ref
    aligned-more?
    aligned-sort
    aligned-sorted-refs
    aligned->datum
    aligned-append-with
    aligned-map)
  (import (asm-3 base))

  (define-annotated (aligned alignment))

  (define (aligned-append-with $ref-append . $list)
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

  (define (aligned->datum $ref->datum $aligned)
    `(aligned
      ,(aligned-alignment $aligned)
      ,($ref->datum (aligned-ref $aligned))))
)
