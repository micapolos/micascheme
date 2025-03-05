(library (typed lang)
  (export tt)
  (import
    (micascheme)
    (typed typed)
    (typed syntax))

  (define-syntax (tt $syntax $lookup)
    (syntax-case $syntax ()
      ((_ expr)
        (typed-value
          (syntax->typed
            (lambda ($type-syntax)
              (eval
                (syntax->datum $type-syntax)
                (environment '(micascheme) '(typed type))))
            $lookup
            #'expr)))))
)
