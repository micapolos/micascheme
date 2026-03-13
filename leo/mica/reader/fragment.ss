(library (leo mica reader fragment)
  (export)
  (import
    (prefix (micascheme) %)
    (only (micascheme) define)
    (prefix (leo fragment) %)
    (mica reader)
    (leo mica reader identifier)
    (leo mica reader literal))

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
