(library (leo mica parser fragment)
  (export)
  (import
    (prefix (micascheme) %)
    (only (micascheme) define)
    (prefix (leo fragment) %)
    (mica parser)
    (leo mica parser identifier)
    (leo mica parser literal))

  (define (line-annotations-fragment $style)
    (annotation literal))
)
