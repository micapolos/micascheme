(library (asm-3 block)
  (export
    block
    block?
    block-alignment
    block-size
    block-labels
    block-blobs

    empty-block
    size-blob-block
    db-block
    dw-block
    identifier-block
    align-block
    offset-block
    list->block
    block-append
    block->datum
    check-block)
  (import
    (asm-3 base)
    (asm-3 binary)
    (asm-3 expression)
    (asm-3 dependent)
    (asm-2 relocable)
    (asm-2 aligned)
    (asm-3 identified)
    (asm-2 alignment)
    (asm-3 size)
    (asm-2 aligned-sized)
    (asm-3 environmental)
    (asm-3 environment)
    (asm-3 sized)
    (asm-3 sized-relocable))

  (define-type label (identified offset))
  (define-type blob (expression binary))
  (data (block alignment size labels blobs))

  (define (empty-block)
    (block 1 0 (stack) (stack)))

  (define (align-block $alignment)
    (block-with-alignment (empty-block) $alignment))

  (define (identifier-block $id)
    (block-with-labels (empty-block)
      (stack (identified $id 0))))

  (define (size-blob-block $size $blob)
    (fluent (empty-block)
      (block-with-size $size)
      (block-with-blobs (stack $blob))))

  (define (db-block . $db-expressions)
    (fluent (empty-block)
      (block-with-size (length $db-expressions))
      (block-with-blobs
        (stack
          (map-expressions
            (lambda ($dbs) #`(db-binary #,@$dbs))
            $db-expressions)))))

  (define (dw-block . $dw-expressions)
    (fluent (empty-block)
      (block-with-size (* 2 (length $dw-expressions)))
      (block-with-blobs
        (stack
          (map-expressions
            (lambda ($dws) #`(dw-binary #,@$dws))
            $dw-expressions)))))

  (define (offset-label $offset $label)
    (identified-map $label (partial + $offset)))

  (define (offset-block $offset $block)
    (block-with-labels $block (map (partial offset-label $offset) (block-labels $block))))

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
                  (pure-expression #`(zero-binary #,$offset))))
              (block-blobs $block)))))
      (empty-block)
      $blocks))

  (define (label->datum $label)
    (identified->entry-datum $label))

  (define (blob->datum $blob)
    (expression->datum $blob))

  (define (block->datum $block)
    `(block
      ,(block-alignment $block)
      ,(block-size $block)
      (stack ,@(map label->datum (reverse (block-labels $block))))
      (stack ,@(map expression->datum (reverse (block-blobs $block))))))

  (define-rule-syntax (check-block block out)
    (check (equal? (block->datum block) 'out)))
)
