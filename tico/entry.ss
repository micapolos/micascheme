(library (tico entry)
  (export
    entry entry? entry-parameters entry-arguments
    typings->entry
    entry-let
    entries-let)
  (import
    (micascheme)
    (tico typing)
    (tico datum))

  (data (entry parameters arguments))

  (define (typings->entry $typings)
    (entry
      (ordered-map typing-parameter $typings)
      $typings))

  (define (entry-let $entry $body)
    (typing-application
      (typing-abstraction
        (entry-parameters $entry)
        $body)
      (entry-arguments $entry)))

  (define (entries-let $entries $body)
    (fold-left
      (lambda ($body $entry)
        (entry-let $entry $body))
      $body
      (reverse $entries)))
)
