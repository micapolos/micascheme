(library (asm-2 aligned)
  (export
    aligned aligned? aligned-alignment aligned-ref
    aligned-more
    aligned-sort)
  (import (micascheme))

  (data (aligned alignment ref))

  (define (aligned-more $aligned-1 $aligned-2)
    (>
      (aligned-alignment $aligned-1)
      (aligned-alignment $aligned-2)))

  (define (aligned-sort $aligned-list)
    (sort aligned-more $aligned-list))
)
