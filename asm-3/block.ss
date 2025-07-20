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
    block->bytevector
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
    (asm-2 aligned-sized)
    (asm-3 environmental)
    (asm-3 environment)
    (asm-3 sized))

  (define-type block (dependent (aligned (sized (relocable (lookable (environmental (stack binary))))))))

  (define (empty-block)
    (pure-dependent
      (aligned 1
        (sized 0
          (pure-relocable
            (pure-lookable
              (pure-environmental
                (stack))))))))

  (define (align-block $alignment)
    (pure-dependent
      (aligned $alignment
        (pure-sized
          (pure-relocable
            (pure-lookable
              (pure-environmental
                (stack))))))))

  (define (bytevector-block $bytevector)
    (pure-dependent
      (pure-aligned
        (sized (bytevector-length $bytevector)
          (pure-relocable
            (pure-lookable
              (pure-environmental
                (stack (bytevector-binary $bytevector)))))))))

  (define (u8-block $u8)
    (pure-dependent
      (pure-aligned
        (sized 1
          (pure-relocable
            (pure-lookable
              (pure-environmental
                (stack (u8-binary $u8)))))))))

  (define (u16-block $u16 $endianness)
    (pure-dependent
      (pure-aligned
        (sized 2
          (pure-relocable
            (pure-lookable
              (pure-environmental
                (stack (u16-binary $u16 $endianness)))))))))

  (define (u8-expression-block $u8-expression)
    (dependent-map $u8-expression
      (lambda ($relocable)
        (pure-aligned
          (sized 1
            (relocable-map $relocable
              (lambda ($lookable)
                (lookable-map $lookable
                  (lambda ($u8)
                    (pure-environmental
                      (stack (u8-binary $u8))))))))))))

  (define (u16-expression-block $u16-expression $endianness)
    (dependent-map $u16-expression
      (lambda ($relocable)
        (pure-aligned
          (sized 2
            (relocable-map $relocable
              (lambda ($lookable)
                (lookable-map $lookable
                  (lambda ($u16)
                    (pure-environmental
                      (stack (u16-binary $u16 $endianness))))))))))))

  (define (identifier-block $identifier)
    (pure-dependent
      (pure-aligned
        (pure-sized
          (relocable-with ($org)
            (pure-lookable
              (environmental
                (environment (list (identified $identifier $org)))
                (stack))))))))

  (define (item-slack $size)
    (pure-relocable
      (pure-lookable
        (pure-environmental
          (stack
            (zero-binary $size))))))

  (define (list->item . $relocable-list)
    (relocable-append-map $relocable-list
      (lambda ($lookable-list)
        (lookable-append-map $lookable-list
          (lambda ($environmental-list)
            (environmental-append-map $environmental-list
              (lambda ($binary-stacks)
                (apply append $binary-stacks))))))))

  (define-list->/append (block $blocks)
    (dependent-map (list->dependent $blocks)
      (lambda ($aligned-sized-list)
        (aligned-map
          (list->aligned-sized $aligned-sized-list item-slack list->item)))))

  (define (block+identifier $block $identifier)
    (dependent-map $block
      (lambda ($aligned)
        (aligned-map $aligned
          (lambda ($sized)
            (sized-map $sized
              (lambda ($relocable)
                (relocable-map $relocable
                  (lambda ($lookable)
                    (lookable-map $lookable
                      (lambda ($environmental)
                        (environmental-update-environment $environmental
                          (lambda ($environment)
                            (environment+ $environment $identifier (sized-size $sized)))))))))))))))

  (define (block+u8-expression $block $expression)
    (dependent-append-map
      (lambda ($aligned $expression-relocable)
        (aligned-map $aligned
          (lambda ($sized)
            (sized-update $sized
              (partial + 1)
              (lambda ($block-relocable)
                (relocable-append-map
                  (lambda ($block-lookable $expression-lookable)
                    (lookable-append-map
                      (lambda ($environmental $u8)
                        (environmental-map $environmental
                          (lambda ($binary-stack)
                            (push $binary-stack (u8-binary $u8)))))
                      $block-lookable $expression-lookable))
                  $block-relocable
                  (relocable+offset $expression-relocable (sized-size $sized))))))))
      $block
      $expression))

  (define (block+u16-expression $block $expression $endianness)
    (dependent-append-map
      (lambda ($aligned $expression-relocable)
        (aligned-map $aligned
          (lambda ($sized)
            (sized-update $sized
              (partial + 2)
              (lambda ($block-relocable)
                (relocable-append-map
                  (lambda ($block-lookable $expression-lookable)
                    (lookable-append-map
                      (lambda ($environmental $u16)
                        (environmental-map $environmental
                          (lambda ($binary-stack)
                            (push $binary-stack (u16-binary $u16 $endianness)))))
                      $block-lookable $expression-lookable))
                  $block-relocable
                  (relocable+offset $expression-relocable (sized-size $sized))))))))
      $block
      $expression))

  (define (block->bytevector $org $lookup $block)
    (fluent $block
      (dependent-ref)
      (aligned-ref)
      (sized-ref)
      (relocable-ref $org)
      (lookable-ref $lookup)
      (environmental-ref)
      (reverse)
      (list->binary)
      (binary->bytevector)))

  (define (block->datum $org $lookup $block)
    (dependent->datum
      (dependent-map $block
        (lambda ($aligned)
          (aligned->datum
            (aligned-map $aligned
              (lambda ($sized)
                (sized->datum
                  (sized-map $sized
                    (lambda ($relocable)
                      (environmental->datum
                        (environmental-map (lookable-ref (relocable-ref $relocable $org) $lookup)
                          (lambda ($binary-stack)
                            `(block
                              ,@(reverse
                                (map binary->datum $binary-stack))))))))))))))))

  (define-rule-syntax (check-block org lookup block out)
    (check (equal? (block->datum org lookup block) 'out)))
)
