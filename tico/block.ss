(library (tico block)
  (export
    block block? block-bindings block-entries
    block-define)
  (import
    (micascheme)
    (tico binding)
    (tico typing)
    (tico entry))

  (data (block bindings entries))

  (define (block-define $block $typing)
    (lets
      ($entry (typing->entry $typing))
      (block
        (push
          (block-bindings $block)
          (binding (entry-parameter $entry)))
        (push
          (block-entries $block)
          $entry))))
)
