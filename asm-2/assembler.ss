(library (asm-2 assembler)
  (export
    assembler assembler? assembler-lookup assembler-org assembler-binary-stack
    empty-assembler
    assembler+identifier
    identifier-assembler
    assembler-bytevector
    assembler-ref?
    assembler-ref)
  (import (micascheme) (syntax lookup) (asm-2 fragment) (asm-2 block))

  (data (assembler lookup org binary-stack))

  (define (empty-assembler $org)
    (assembler (empty-lookup) $org (stack)))

  (define (assembler-ref? $assembler $identifier)
    ((assembler-lookup $assembler) $identifier))

  (define (assembler-ref $assembler $identifier)
    (lookup-ref (assembler-lookup $assembler) $identifier))

  (define (assembler+value $assembler $identifier $value)
    (assembler-with-lookup $assembler
      (lookup+ (assembler-lookup $assembler) $identifier $value)))

  (define (assembler+identifier $lookup $assembler $identifier)
    (switch (assembler-ref? $assembler $identifier)
      ((false? _)
        (switch (lookup-ref $lookup $identifier)
          ((fragment? $fragment)
            (lets
              ($assembler
                (fold-left
                  (partial assembler+identifier $lookup)
                  $assembler
                  (fragment-deps $fragment)))
              ($org (assembler-org $assembler))
              ($assembler-lookup (assembler-lookup $assembler))
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
                  (assembler+value $assembler $identifier $value)))))
          ((else $value)
            (assembler+value $assembler $identifier $value))))
      ((else _) $assembler)))

  (define (identifier-assembler $lookup $identifier $org)
    (assembler+identifier $lookup (empty-assembler $org) $identifier))

  (define (assembler-bytevector $assembler)
    (binary->bytevector
      (list->binary
        (reverse
          (assembler-binary-stack $assembler)))))
)
