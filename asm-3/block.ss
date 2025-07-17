 (library (asm-3 block)
  (export
    block? block-alignment block-size

    empty-block
    block+label
    block+define
    block+binary
    block+zeros
    block-align
    block->aligned-sized-relocable-binary-syntax
    block->datum
    check-block)
  (import
    (micascheme)
    (syntax lookup)
    (asm-2 relocable)
    (asm-3 identified)
    (asm-2 aligned)
    (asm-3 sized))

  (data
    (block
      alignment
      size
      identified-offset-stack
      identified-expression-syntax-stack
      relocable-binary-syntax-stack))

  (define (empty-block)
    (block 1 0 (stack) (stack) (stack)))

  (define (block+label $block $label)
    (block-with-identified-offset-stack $block
      (push
        (block-identified-offset-stack $block)
        (identified $label (block-size $block)))))

  (define (block+define $block $identifier $syntax)
    (block-with-identified-expression-syntax-stack $block
      (push
        (block-identified-expression-syntax-stack $block)
        (identified $identifier $syntax))))

  (define (block+binary $block $size $relocable-binary-syntax)
    (fluent $block
      (block-with-relocable-binary-syntax-stack
        (push
          (block-relocable-binary-syntax-stack $block)
          (relocable+offset $relocable-binary-syntax (block-size $block))))
      (block-with-size
        (+ (block-size $block) $size))))

  (define (block+zeros $block $size)
    (if (zero? $size)
      $block
      (block+binary $block $size
        (relocable-with
          #`(zero-binary #,(literal->syntax $size))))))

  (define (block-align $block $alignment)
    (lets
      ($size (block-size $block))
      (fluent $block
        (block-with-alignment (max (block-alignment $block) $alignment))
        (block+zeros (- (bitwise-align $size $alignment) $size)))))

  (define (block->aligned-sized-relocable-binary-syntax $block)
    (aligned (block-alignment $block)
      (sized (block-size $block)
        (relocable-with ($org)
          #`(lets
            #,@(map-with ($identified-offset (reverse (block-identified-offset-stack $block)))
              #`(
                #,(identified-identifier $identified-offset)
                #,(literal->syntax (+ $org (identified-ref $identified-offset)))))
            #,@(map-with ($identified-expression-syntax (reverse (block-identified-expression-syntax-stack $block)))
              #`(
                #,(identified-identifier $identified-expression-syntax)
                #,(identified-ref $identified-expression-syntax)))
            (binary-append
              #,@(relocable-ref
                (list->relocable
                  (reverse
                    (block-relocable-binary-syntax-stack $block)))
                $org)))))))

  (define (block->datum $block $org)
    `(block
      (alignment ,(block-alignment $block))
      (size ,(block-size $block))
      ,(syntax->datum
        (relocable-ref
          (sized-ref
            (aligned-ref
              (block->aligned-sized-relocable-binary-syntax $block)))
          $org))))

  (define-rule-syntax (check-block in org out)
    (check (equal? (block->datum in org) 'out)))
)
