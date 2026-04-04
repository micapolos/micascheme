(library (leo logging)
  (export logging)
  (import
    (only (scheme) let quasiquote unquote)
    (syntaxes)
    (leo write))

  (define-rules-syntax
    ((logging (label x))
      (let ((val x))
        (write `(label ,val))
        val))
    ((logging x)
      (let ((val x))
        (write val)
        val)))
)
