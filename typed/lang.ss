(library (typed lang)
  (export
    tt
    any-boolean any-string any-number any-syntax any-lambda
    syntax lambda)
  (import
    (micascheme)
    (any)
    (typed type)
    (typed typed)
    (typed syntax)
    (typed keywords)
    (typed phased))

  (define-syntax (tt $syntax $lookup)
    (syntax-case $syntax ()
      ((_ expr)
        (typed-value
          (syntax->typed
            (lambda ($type-syntax)
              (eval
                (syntax->datum $type-syntax)
                (environment '(micascheme) '(typed type))))
            0
            #'expr)))))
)
