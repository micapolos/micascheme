(library (asm-3 op)
  (export
    empty-block
    block+op
    u8-op
    block->bytevector
    op-append
    list->op)
  (import
    (asm-3 base)
    (asm-3 expression)
    (asm-3 dependent)
    (asm-2 relocable)
    (asm lookable)
    (asm-2 aligned)
    (asm-3 sized))

  ; block -> dependent-aligned-sized-relocable-lookable-binary-stack
  ; op -> (lambda (block) block)

  (define-rule-syntax (op (block) body)
    (lambda (block) body))

  (define (empty-block)
    (pure-dependent
      (aligned 1
        (sized 0
          (pure-relocable
            (pure-lookable
              (stack)))))))

  (define (u8-op $expression)
    (op ($block)
      (dependent-append-with
        (lambda ($aligned $expression-relocable)
          (aligned-map
            (lambda ($sized)
              (sized-map
                (lambda ($block-relocable)
                  (relocable-append-with
                    (lambda ($block-lookable $expression-lookable)
                      (lookable-append-with
                        (lambda ($binary-stack $u8)
                          (push $binary-stack (u8-binary $u8)))
                        $block-lookable $expression-lookable))
                    $block-relocable $expression-relocable))
                $sized))
            $aligned))
        $block
        $expression)))

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
      (reverse)
      (list->binary)
      (binary->bytevector)))
)
