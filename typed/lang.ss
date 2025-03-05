(library (typed lang)
  (export tt a an : assume)
  (import
    (micascheme)
    (typed typed)
    (typed syntax)
    (typed keywords))

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
