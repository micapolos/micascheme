(library (asm-3 op)
  (export
    empty-block
    block+op
    u8-op
    u16-op
    block->bytevector
    op->bytevector
    op-append
    list->op)
  (import
    (asm-3 base)
    (asm-3 expression)
    (asm-3 dependent)
    (asm-2 relocable)
    (asm lookable)
    (asm-2 aligned)
    (asm-3 environmental)
    (asm-3 sized))

  ; block -> dependent-aligned-sized-relocable-lookable-environmental-binary-stack
  ; op -> (lambda (block) block)

  (define-rule-syntax (op (block) body)
    (lambda (block) body))

  (define (empty-block)
    (pure-dependent
      (aligned 1
        (sized 0
          (pure-relocable
            (pure-lookable
              (pure-environmental
                (stack))))))))

  (define (block+u8-expression $block $expression)
    (dependent-append-with
      (lambda ($aligned $expression-relocable)
        (aligned-map
          (lambda ($sized)
            (sized+size
              (sized-map
                (lambda ($block-relocable)
                  (relocable-append-with
                    (lambda ($block-lookable $expression-lookable)
                      (lookable-append-with
                        (lambda ($environmental $u8)
                          (environmental-map
                            (lambda ($binary-stack)
                              (push $binary-stack (u8-binary $u8)))
                            $environmental))
                        $block-lookable $expression-lookable))
                    $block-relocable
                    (relocable+offset $expression-relocable (sized-size $sized))))
                $sized)
              1))
          $aligned))
      $block
      $expression))

  (define (block+u16-expression $block $expression $endianness)
    (dependent-append-with
      (lambda ($aligned $expression-relocable)
        (aligned-map
          (lambda ($sized)
            (sized+size
              (sized-map
                (lambda ($block-relocable)
                  (relocable-append-with
                    (lambda ($block-lookable $expression-lookable)
                      (lookable-append-with
                        (lambda ($environmental $u16)
                          (environmental-map
                            (lambda ($binary-stack)
                              (push $binary-stack (u16-binary $u16 $endianness)))
                            $environmental))
                        $block-lookable $expression-lookable))
                    $block-relocable
                    (relocable+offset $expression-relocable (sized-size $sized))))
                $sized)
              2))
          $aligned))
      $block
      $expression))

  (define (u8-op $expression)
    (op ($block)
      (block+u8-expression $block $expression)))

  (define (u16-op $expression $endianness)
    (op ($block)
      (block+u16-expression $block $expression $endianness)))

  (define (block+op $block $op)
    ($op $block))

  (define-list->/append (op $ops)
    (op ($block)
      (fold-left block+op $block $ops)))

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

  (define (op->bytevector $org $lookup $op)
    (block->bytevector $org $lookup (block+op (empty-block) $op)))
)
