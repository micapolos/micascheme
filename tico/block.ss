(library (tico block)
  (export
    block block? block-entries block-args
    empty-block
    block-update-entries
    block-update-args
    block+arg
    block-let
    block-struct)
  (import
    (micascheme)
    (tico binding)
    (tico typing)
    (tico entry))

  (data (block entries args))

  (define (empty-block)
    (block (stack) (stack)))

  (define (block-update-entries $block $fn)
    (block
      ($fn (block-entries $block))
      (block-args $block)))

  (define (block-update-args $block $fn)
    (block
      (block-entries $block)
      ($fn (block-args $block))))

  (define (block+arg $block $arg)
    (block-update-args $block
      (lambda ($args)
        (push $args $arg))))

  (define (block-let $block $fn)
    (entries-let
      (block-entries $block)
      ($fn (block-args $block))))

  (define (block-struct $name $block)
    (block-let $block (partial typing-struct $name)))
)
