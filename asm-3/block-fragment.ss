(library (asm-3 block-fragment)
  (export block->fragment)
  (import
    (asm-3 base)
    (asm-3 dependent)
    (asm-3 expression)
    (asm-2 aligned)
    (asm-3 sized)
    (asm-3 block)
    (asm-3 identified)
    (asm-2 relocable)
    (asm lookable))

  (define (block->fragment $block)
    (lets
      ($alignment (block-alignment $block))
      ($size (block-size $block))
      ($labels (reverse (block-labels $block)))
      ($label-identifiers (map identified-identifier $labels))
      ($binary-expressions (reverse (block-blobs $block)))
      ($binary-expression (combine-expressions list->binary $binary-expressions))
      ($dependencies
        (remp
          (lambda ($identifier)
            (memp (partial free-identifier=? $identifier) $label-identifiers))
          (dependent-identifiers $binary-expression)))
      (dependent $dependencies
        (aligned (block-alignment $block)
          (sized (block-size $block)
            (relocable-with ($org)
              (lookable ($lookup)
                (lookable-ref
                  (relocable-ref (dependent-ref $binary-expression) $org)
                  (org-lookup+labels $org $lookup $labels)))))))))
)
