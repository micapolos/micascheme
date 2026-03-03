(library (leo2 script)
  (export
    datum->script
    leo-script)
  (import
    (leo2 base)
    (leo writing-reader)
    (writing)
    (leo reader))

  (define (datum->script $datum)
    (writing-string
      (reader-end
        (reader-read-list (writing-reader) $datum))))

  (define-rule-syntax (leo-script x ...)
    (datum->script '(x ...)))
)
