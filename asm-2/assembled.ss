(library (asm-2 assembled)
  (export
    assembled assembled? assembled-label-lookup assembled-org assembled-binary-stack
    assembled+fragment
    fragment->bytevector)
  (import (micascheme) (syntax lookup) (asm-2 fragment) (asm-2 block) (asm-2 assembler))

  (data (assembled label-lookup org binary-stack))

  (define (empty-assembled $org)
    (assembled (empty-lookup) $org (stack)))

  (define (assembled+fragment $lookup $assembled $label)
    (lets
      ($label-lookup (assembled-label-lookup $assembled))
      (switch ($label-lookup $label)
        ((false? _)
          (lets
            ($fragment (lookup-ref $lookup $label))
            ($assembled
              (fold-left
                (partial assembled+fragment $lookup)
                $assembled
                (fragment-deps $fragment)))
            ($org (assembled-org $assembled))
            ($block (fragment-ref $fragment $label-lookup))
            ($size (block-size $block))
            ($binary (block->binary $block $org))
            (assembled
              (lookup+ $label-lookup $label $org)
              (+ $org $size)
              (push (assembled-binary-stack $assembled) $binary))))
        ((else _) $assembled))))

  (define (fragment->bytevector $lookup $label $org)
    (binary->bytevector
      (list->binary
        (reverse
          (assembled-binary-stack
            (assembled+fragment $lookup (empty-assembled $org) $label))))))
)
