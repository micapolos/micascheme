(library (asm alignment)
  (export
    empty-alignment
    alignment-append
    list->alignment)
  (import (asm base))

  (define (empty-alignment) 1)

  (define-list->/append (alignment $alignments)
    (apply max 1 $alignments))
)
