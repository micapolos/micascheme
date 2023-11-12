(library (tico block)
  (export
    block block? block-entries block-typings
    empty-block
    block-update-entries
    block-update-typings
    block-push-typing
    block->typing)
  (import
    (micascheme)
    (tico binding)
    (tico typing)
    (tico entry))

  (data (block entries typings))

  (define (empty-block)
    (block (stack) (stack)))

  (define (block-update-entries $block $fn)
    (block
      ($fn (block-entries $block))
      (block-typings $block)))

  (define (block-update-typings $block $fn)
    (block
      (block-entries $block)
      ($fn (block-typings $block))))

  (define (block-push-typing $block $typing)
    (block-update-typings $block
      (lambda ($typings)
        (push $typings $typing))))

  (define (block->typing $block $fn)
    (entries-let
      (block-entries $block)
      ($fn (block-typings $block))))
)
