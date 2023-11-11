(library (tico block)
  (export
    block block? block-bindings block-entries
    block-use-typings)
  (import
    (micascheme)
    (tico binding)
    (tico typing)
    (tico entry))

  (data (block bindings entries))

  (define (block-use-typings $block $typings)
    (lets
      ($entry (typings->entry $typings))
      (block
        (push-list
          (block-bindings $block)
          (map binding (map entry-parameters $entry)))
        (push
          (block-entries $block)
          $entry))))

  (define (block-begin $block)
    (block
      (block-bindings $block)
      (stack)))
)
