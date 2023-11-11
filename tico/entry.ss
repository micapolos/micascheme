(library (tico entry)
  (export
    entry entry? entry-parameters entry-arguments
    typings->entry)
  (import
    (micascheme)
    (tico typing))

  (data (entry parameters arguments))

  (define (typings->entry $typings)
    (entry
      (ordered-map typing-parameter $typings)
      $typings))
)
