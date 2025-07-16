(library (asm-2 assembler)
  (export
    assembler assembler? assembler-lookup assembler-org assembler-binary-stack
    empty-assembler
    assembler+identifier
    identifier-assembler
    assembler-bytevector
    assembler-ref?
    assembler-ref)
  (import (micascheme) (syntax lookup) (asm-2 relocable) (asm-2 fragment) (asm-2 block))

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

  (define (assembler+binary $assembler $size $binary)
    (assembler
      (assembler-lookup $assembler)
      (+ (assembler-org $assembler) $size)
      (push (assembler-binary-stack $assembler) $binary)))

  (define (assembler+relocable-value $assembler $identifier $relocable)
    (assembler+value $assembler $identifier
      (relocable-ref $relocable (assembler-org $relocable))))

  (define (assembler+block $assembler $identifier $block)
    (lets
      ($size (block-size $block))
      ($org (assembler-org $assembler))
      ($binary-stack (assembler-binary-stack $assembler))
      ($binary (block->binary $block $org))
      (assembler+binary
        (assembler+value $assembler $identifier $org)
        $size $binary)))

  (define (assembler+fragment $lookup $assembler $identifier $fragment)
    (lets
      ($assembler
        (fold-left
          (partial assembler+identifier $lookup)
          $assembler
          (fragment-deps $fragment)))
      (assembler+obj $lookup $assembler $identifier
        (fragment-ref $fragment (assembler-lookup $assembler)))))

  (define (assembler+obj $lookup $assembler $identifier $obj)
    (switch $obj
      ((fragment? $fragment)
        (assembler+fragment $lookup $assembler $identifier $fragment))
      ((block? $block)
        (assembler+block $assembler $identifier $block))
      ((relocable? $relocable)
        (assembler+relocable-value $assembler $identifier $relocable))
      ((else $value)
        (assembler+value $assembler $identifier $value))))

  (define (assembler+identifier $lookup $assembler $identifier)
    (switch (assembler-ref? $assembler $identifier)
      ((false? _)
        (assembler+obj $lookup $assembler $identifier
          (lookup-ref $lookup $identifier)))
      ((else _) $assembler)))

  (define (identifier-assembler $lookup $identifier $org)
    (assembler+identifier $lookup (empty-assembler $org) $identifier))

  (define (assembler-bytevector $assembler)
    (binary->bytevector
      (list->binary
        (reverse
          (assembler-binary-stack $assembler)))))
)
