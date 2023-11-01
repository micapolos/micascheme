(library (tico typed)
  (export
    typed typed? typed-type typed-value
    typed-dynamic?
    typed-list->dynamic-values)
  (import
    (micascheme)
    (tico type))

  (data (typed type value))

  (define (typed-dynamic? $typed)
    (and (type-dynamic? (typed-type $typed)) $typed))

  (define (typed-list->dynamic-values $typed-list)
    (map typed-value (filter typed-dynamic? $typed-list)))
)
