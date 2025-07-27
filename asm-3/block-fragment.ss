(library (asm-3 block-fragment)
  (export block->fragment)
  (import
    (asm-3 base)
    (asm-3 dependent)
    (asm-3 expression)
    (asm-3 aligned)
    (asm-3 sized)
    (asm-3 block)
    (asm-3 identified)
    (asm-3 relocable))

  (define (block->fragment $block)
    (lets
      ($alignment (block-alignment $block))
      ($size (block-size $block))
      ($labels (reverse (block-labels $block)))
      ($label-let-entries
        (map
          (lambda ($label)
            #`(
              #,(identified-identifier $label)
              (+ $org #,(identified-ref $label))))
          $labels))
      ($label-identifiers (map identified-identifier $labels))
      ($binary-expression
        (map-expressions
          (lambda ($expressions)
            #`(binary-append #,@$expressions))
          (reverse (block-blobs $block))))
      ($dependencies
        (remp
          (lambda ($identifier)
            (memp (partial free-identifier=? $identifier) $label-identifiers))
          (dependent-identifiers $binary-expression)))
      (dependent $dependencies
        (aligned (block-alignment $block)
          (sized (block-size $block)
            #`(relocable-with ($org)
              (let
                (#,@$label-let-entries)
                #,(dependent-ref $binary-expression))))))))
)
