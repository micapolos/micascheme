(library (leo mica parser fragment)
  (export)
  (import
    (prefix (micascheme) %)
    (only (micascheme) define)
    (prefix (leo fragment) %)
    (mica parser)
    (leo mica parser identifier))

  (define (line-annotations-fragment $style)
    (one-of
      (annotation identifier)))
)
