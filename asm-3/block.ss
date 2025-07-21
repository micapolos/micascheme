(library (asm-3 block)
  (export
    empty-block
    u8-block
    u16-block
    u8-expression-block
    u16-expression-block
    identifier-block
    align-block
    bytevector-block
    list->block
    block-append
    block->datum
    check-block)
  (import
    (asm-3 base)
    (asm-3 expression)
    (asm-3 dependent)
    (asm-2 relocable)
    (asm lookable)
    (asm-2 aligned)
    (asm-3 identified)
    (asm-2 alignment)
    (asm-3 size)
    (asm-2 aligned-sized)
    (asm-3 environmental)
    (asm-3 environment)
    (asm-3 sized)
    (asm-3 sized-relocable))

  (define-type label (identified (relocable offset)))
  (define-type blob (expression binary))
  (data (block alignment size labels blobs))

  (define (empty-block)
    (block 1 0 (stack) (stack)))

  (define (align-block $alignment)
    (block-with-alignment (empty-block) $alignment))

  (define (size-binary-expression-block $size $binary-expression)
    (fluent (empty-block)
      (block-with-size $size)
      (block-with-blobs (stack $binary-expression))))

  (define (u8-expression-block $u8-expression)
    (size-binary-expression-block 1
      (expression-map $u8-expression u8-binary)))

  (define (u16-expression-block $u16-expression $endianness)
    (size-binary-expression-block 2
      (expression-map $u16-expression
        (partial-flip u16-binary $endianness))))

  (define (u8-block $u8)
    (size-binary-expression-block 1 (pure-expression (u8-binary $u8))))

  (define (u16-block $u16 $endianness)
    (size-binary-expression-block 2 (pure-expression (u16-binary $u16 $endianness))))

  (define (bytevector-block $bytevector)
    (size-binary-expression-block
      (bytevector-length $bytevector)
      (pure-expression (bytevector-binary $bytevector))))

  (define (identifier-block $identifier)
    (block-with-labels (empty-block)
      (stack (identified $identifier (org-relocable)))))

  (define (list->relocable-item $relocable-list)
    (relocable-map (list->relocable $relocable-list)
      (lambda ($lookable-list)
        (lookable-map (list->lookable $lookable-list)
          (lambda ($environmental-list)
            (environmental-map (list->environmental $environmental-list)
              (lambda ($binary-stacks)
                (reverse (flatten (map reverse $binary-stacks))))))))))

  (define (offset-label $offset $label)
    (map-identified (partial offset-relocable $offset) $label))

  (define (offset-blob $offset $blob)
    (offset-expression $offset $blob))

  (define-list->/append (block $blocks)
    (fold-left
      (lambda ($folded $block)
        (lets
          ($alignment (alignment-append (block-alignment $folded) (block-alignment $block)))
          ($aligned-size (bitwise-align (block-size $folded) (block-alignment $block)))
          ($offset (- $aligned-size (block-size $folded)))
          (block
            $alignment
            (size+ $aligned-size (block-size $block))
            (push-all
              (block-labels $folded)
              (map (partial offset-label $aligned-size) (block-labels $block)))
            (push-all
              (if (zero? $offset)
                (block-blobs $folded)
                (push
                  (block-blobs $folded)
                  (pure-expression (zero-binary $offset))))
              (map (partial offset-blob $aligned-size) (block-blobs $block))))))
      (empty-block)
      $blocks))

  (define (label->datum $org $label)
    (identified->entry-datum (identified-map $label (partial locate-relocable $org))))

  (define (blob->datum $org $lookup $blob)
    (cadr (expression->datum $org $lookup (expression-map $blob binary->datum))))

  (define (block->datum $org $lookup $block)
    `(block
      (alignment ,(block-alignment $block))
      (size ,(block-size $block))
      (labels ,@(map (partial label->datum $org) (reverse (block-labels $block))))
      (blobs ,@(map (partial blob->datum $org $lookup) (reverse (block-blobs $block))))))

  (define-rule-syntax (check-block org lookup block out)
    (check (equal? (block->datum org lookup block) 'out)))
)
