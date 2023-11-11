(library (tico block)
  (export
    block block? block-entries block-typings
    empty-block)
  (import
    (micascheme)
    (tico binding)
    (tico typing)
    (tico entry))

  (data (block entries typings))

  (define (empty-block)
    (block (stack) (stack)))
)
