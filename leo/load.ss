(library (leo load)
  (export leo-load)
  (import
    (micascheme)
    (getter)
    (leo getter))

  (define (leo-load $path)
    (eval
      `(top-level-program ,@(getter-load! line-annotations-getter $path))
      (copy-environment (environment '(micascheme)) #t)))
)
