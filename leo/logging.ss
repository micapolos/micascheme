(library (leo logging)
  (export logging)
  (import
    (except (scheme) let write)
    (syntaxes)
    (leo let)
    (leo in)
    (leo write))

  (define-rules-syntax
    ((logging (label x))
      (let
        (val x)
        (in
          (write `(label ,val))
          val)))
    ((logging x)
      (let
        (val x)
        (in
          (write val)
          val))))
)
