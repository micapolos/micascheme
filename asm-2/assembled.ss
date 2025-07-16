(library (asm-2 assembled)
  (export
    assembled assembled? assembled-lookup assembled-org assembled-binary-stack
    assembled+fragment
    fragment->bytevector)
  (import (micascheme) (syntax lookup) (asm-2 fragment) (asm-2 block))

  (data (assembled lookup org binary-stack))

  (define (empty-assembled $org)
    (assembled (empty-lookup) $org (stack)))

  (define (assembled+fragment $lookup $assembled $identifier)
    (lets
      ($assembled-lookup (assembled-lookup $assembled))
      (switch ($assembled-lookup $identifier)
        ((false? _)
          (lets
            ($fragment (lookup-ref $lookup $identifier))
            ($assembled
              (fold-left
                (partial assembled+fragment $lookup)
                $assembled
                (fragment-deps $fragment)))
            ($org (assembled-org $assembled))
            ($block (fragment-ref $fragment $assembled-lookup))
            ($size (block-size $block))
            ($binary (block->binary $block $org))
            (assembled
              (lookup+ $assembled-lookup $identifier $org)
              (+ $org $size)
              (push (assembled-binary-stack $assembled) $binary))))
        ((else _) $assembled))))

  (define (fragment->bytevector $lookup $identifier $org)
    (binary->bytevector
      (list->binary
        (reverse
          (assembled-binary-stack
            (assembled+fragment $lookup (empty-assembled $org) $identifier))))))
)
