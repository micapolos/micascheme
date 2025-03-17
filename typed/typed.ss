(library (typed typed)
  (export
    typed typed? typed-type typed-value
    type->typed
    typed-map-value)
  (import
    (micascheme)
    (any))

  (data (typed type value))

  (define (type->typed $type)
    (typed $type any-type))

  (define (typed-map-value $typed $fn)
    (typed
      (typed-type $typed)
      ($fn (typed-value $typed))))
)
