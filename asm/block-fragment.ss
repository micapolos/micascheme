(library (asm block-fragment)
  (export block->fragment)
  (import
    (asm base)
    (asm dependent)
    (asm expression)
    (asm aligned)
    (asm sized)
    (asm block)
    (asm identified)
    (asm fragment)
    (asm relocable))

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
      (fragment-pad
        (dependent $dependencies
          (aligned (block-alignment $block)
            (sized (block-size $block)
              #`(relocable-with ($org)
                (let
                  (#,@$label-let-entries)
                  #,(dependent-ref $binary-expression)))))))))
)
