(library (asm-2 assembled)
  (export
    assembled assembled? assembled-label-lookup assembled-org assembled-blob-stack
    assembled+fragment
    fragment->bytevector)
  (import (micascheme) (syntax lookup) (asm-2 fragment) (asm-2 block) (asm-2 assembler))

  (data (assembled label-lookup org blob-stack))

  (define (empty-assembled $org)
    (assembled (empty-lookup) $org (stack)))

  (define (assembled+fragment $lookup $assembled $label)
    (lets
      ($label-lookup (assembled-label-lookup $assembled))
      (switch ($label-lookup $label)
        ((false? _)
          (lets
            ($org (assembled-org $assembled))
            ($fragment (lookup-ref $lookup $label))
            ($block (fragment-ref $fragment $label-lookup))
            ($blob (block->blob $block $org))
            (fold-left
              (partial assembled+fragment $lookup)
              (assembled
                (lookup+ $label-lookup $label $org)
                (+ $org (blob-size $blob))
                (push (assembled-blob-stack $assembled) $blob))
              (fragment-deps $fragment))))
        ((else _) $assembled))))

  (define (fragment->bytevector $lookup $label $org)
    (blob->bytevector
      (list->blob
        (reverse
          (assembled-blob-stack
            (assembled+fragment $lookup (empty-assembled $org) $label))))))
)
