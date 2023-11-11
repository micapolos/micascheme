(library (tico entry)
  (export
    entry entry? entry-parameters entry-arguments
    typings->entry)
  (import
    (micascheme)
    (tico typing)
    (tico datum))

  (data (entry parameters arguments))

  (define (typings->entry $typings)
    (entry
      (ordered-map typing-parameter $typings)
      $typings))

  (define (typing-let-entry-datum $parameter-typing $argument-typing)
    `(
      ,(typing-datum $parameter-typing)
      ,(typing-datum $argument-typing)))

  (define (entry-let-datum $entry $body-typing)
    (let-datum
      (map typing-let-entry-datum
        (entry-parameters $entry)
        (entry-arguments $entry))
      (typing-datum $body-typing)))
)
