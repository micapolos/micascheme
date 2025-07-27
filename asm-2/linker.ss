(library (asm-2 linker)
  (export
    linker linker? linker-lookup linker-org linker-binary-stack
    empty-linker
    linker+identifier
    identifier-linker
    linker-bytevector
    linker-ref?
    linker-ref)
  (import
    (micascheme)
    (syntax lookup)
    (asm-2 label)
    (asm-3 relocable)
    (asm-3 relocable)
    (asm-2 fragment)
    (asm-2 block)
    (asm-2 local))

  (data (linker lookup org binary-stack))

  (define (empty-linker $org)
    (linker (empty-lookup) $org (stack)))

  (define (linker-ref? $linker $identifier)
    ((linker-lookup $linker) $identifier))

  (define (linker-ref $linker $identifier)
    (lookup-ref (linker-lookup $linker) $identifier))

  (define (linker+value $linker $identifier $value)
    (linker-with-lookup $linker
      (lookup+ (linker-lookup $linker) $identifier $value)))

  (define (linker+label $linker $label)
    (linker+value $linker
      (label-identifier $label)
      (relocable-with ($org) $org)))

  (define (linker+binary $linker $size $binary)
    (linker
      (linker-lookup $linker)
      (+ (linker-org $linker) $size)
      (push (linker-binary-stack $linker) $binary)))

  (define (linker+relocable-value $linker $identifier $relocable)
    (linker+value $linker $identifier
      (relocable-ref $relocable (linker-org $relocable))))

  (define (linker+local $linker $local)
    (linker-with-lookup $linker
      (local-apply $local (linker-lookup $linker))))

  (define (linker+block $linker $identifier $block)
    (lets
      ($size (block-size $block))
      ($org (linker-org $linker))
      ($binary-stack (linker-binary-stack $linker))
      ($binary (block->binary $block $org))
      (linker+binary
        (linker+value $linker $identifier $org)
        $size $binary)))

  (define (linker+fragment $lookup $linker $identifier $fragment)
    (lets
      ($linker
        (fold-left
          (partial linker+identifier $lookup)
          $linker
          (fragment-deps $fragment)))
      (linker+obj $lookup $linker $identifier
        (fragment-ref $fragment (linker-lookup $linker)))))

  (define (linker+obj $lookup $linker $identifier $obj)
    (switch $obj
      ((fragment? $fragment)
        (linker+fragment $lookup $linker $identifier $fragment))
      ((local? $local)
        (linker+local $linker $local))
      ((block? $block)
        (linker+block $linker $identifier $block))
      ((relocable? $relocable)
        (linker+relocable-value $linker $identifier $relocable))
      ((label? $label)
        (linker+label $linker $label))
      ((else $value)
        (linker+value $linker $identifier $value))))

  (define (linker+identifier $lookup $linker $identifier)
    (switch (linker-ref? $linker $identifier)
      ((false? _)
        (linker+obj $lookup $linker $identifier
          (lookup-ref $lookup $identifier)))
      ((else _) $linker)))

  (define (identifier-linker $lookup $identifier $org)
    (linker+identifier $lookup (empty-linker $org) $identifier))

  (define (linker-bytevector $linker)
    (binary->bytevector
      (list->binary
        (reverse
          (linker-binary-stack $linker)))))
)
