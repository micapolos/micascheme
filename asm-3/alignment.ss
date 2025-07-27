(library (asm-3 alignment)
  (export
    empty-alignment
    alignment-append
    list->alignment)
  (import (asm-3 base))

  (define (empty-alignment) 1)

  (define-list->/append (alignment $alignments)
    (apply max 1 $alignments))
)
