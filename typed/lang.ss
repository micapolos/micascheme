(library (typed lang)
  (export
    tt
    any any-any-lambda any-type
    any-boolean any-string any-number any-syntax any-lambda
    type typeof
    syntax lambda
    define-phased)
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
          (syntax->typed 0
            (lambda ($identifier)
              ($lookup $identifier #'phased))
            #'expr)))))

  (define-rule-syntax (define-phased name expr)
    (begin
      (define-aux-keyword name)
      (define-property name phased expr)))
)
