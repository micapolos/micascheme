(library (leo printing)
  (export printing)
  (import
    (only (scheme) let quasiquote unquote)
    (syntaxes)
    (leo print))

  (define-rules-syntax
    ((printing label x)
      (let ((val x))
        (print `(label ,val))
        val))
    ((printing x)
      (let ((val x))
        (print val)
        val)))
)
