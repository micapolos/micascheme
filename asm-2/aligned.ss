(library (asm-2 aligned)
  (export
    aligned aligned? aligned-alignment aligned-ref
    aligned-more?
    aligned-sort
    aligned-sorted-refs
    aligned->datum
    aligned-map)
  (import (micascheme))

  (data (aligned alignment ref))

  (define (aligned-more? $aligned-1 $aligned-2)
    (>
      (aligned-alignment $aligned-1)
      (aligned-alignment $aligned-2)))

  (define (aligned-sort $aligned-list)
    (sort aligned-more? $aligned-list))

  (define (aligned-sorted-refs $aligned-list)
    (map aligned-ref (aligned-sort $aligned-list)))

  (define (aligned-map $proc $aligned)
    (aligned-with-ref $aligned
      ($proc (aligned-ref $aligned))))

  (define (aligned->datum $ref->datum $aligned)
    `(aligned
      ,(aligned-alignment $aligned)
      ,($ref->datum (aligned-ref $aligned))))
)
