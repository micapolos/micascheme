(library (tico entry)
  (export
    entry entry? entry-parameter entry-argument
    typing->entry)
  (import
    (micascheme)
    (tico typing))

  (data (entry parameter argument))

  (define (typing->entry $typing)
    (entry
      (typing-parameter $typing)
      $typing))
)
