(library (typed typed)
  (export
    typed typed? typed-type typed-value
    type->typed)
  (import
    (micascheme)
    (any))

  (data (typed type value))

  (define (type->typed $type)
    (typed $type any-type))
)
