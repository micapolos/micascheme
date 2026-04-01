(library (leo logging)
  (export logging)
  (import
    (rename
      (except (scheme) write let)
      (list %list))
    (syntaxes)
    (leo let)
    (leo write))

  (define null '())
  (define closed-list %list)
  (define open-list list*)

  (define-rules-syntax
    (keywords and)

    ((logging x)
      (let
        (val x)
        (in
          (write val)
          val)))
    ((logging label x)
      (let
        (val x)
        (in
          (write `(label ,val))
          val))))
)
