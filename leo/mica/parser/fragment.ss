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
    (switch (annotation literal)
      ((%symbol? $symbol)
        (%style-switch $style
          ((%inline-style? _) (%todo))
          ((%colon-style? _) (%todo))
          ((%block-style? _) (%todo))))
      ((else $other)
        (%style-switch $style
          ((%inline-style? _) (%todo))
          ((%colon-style? _) (%todo))
          ((%block-style? _) (%todo))))))
)
