(library (asm-2 assembler)
  (export
    assembler assembler? assembler-lookup assembler-org assembler-binary-stack
    empty-assembler
    assembler+identifier
    identifier-assembler
    assembler-bytevector
    assembler-ref)
  (import (micascheme) (syntax lookup) (asm-2 fragment) (asm-2 block))

  (data (assembler lookup org binary-stack))

  (define (empty-assembler $org)
    (assembler (empty-lookup) $org (stack)))

  (define (assembler+identifier $lookup $assembler $identifier)
    (lets
      ($assembler-lookup (assembler-lookup $assembler))
      (switch ($assembler-lookup $identifier)
        ((false? _)
          (lets
            ($fragment (lookup-ref $lookup $identifier))
            ($assembler
              (fold-left
                (partial assembler+identifier $lookup)
                $assembler
                (fragment-deps $fragment)))
            ($org (assembler-org $assembler))
            ($binary-stack (assembler-binary-stack $assembler))
            (switch (fragment-ref $fragment $assembler-lookup)
              ((block? $block)
                (lets
                  ($size (block-size $block))
                  ($binary (block->binary $block $org))
                  (assembler
                    (lookup+ $assembler-lookup $identifier $org)
                    (+ $org $size)
                    (push $binary-stack $binary))))
              ((else $value)
                (assembler
                  (lookup+ $assembler-lookup $identifier $value)
                  $org
                  $binary-stack)))))
        ((else _) $assembler))))

  (define (identifier-assembler $lookup $identifier $org)
    (assembler+identifier $lookup (empty-assembler $org) $identifier))

  (define (assembler-bytevector $assembler)
    (binary->bytevector
      (list->binary
        (reverse
          (assembler-binary-stack $assembler)))))

  (define (assembler-ref $assembler $identifier)
    (lookup-ref (assembler-lookup $assembler) $identifier))
)
