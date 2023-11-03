(library (tico typed)
  (export
    typed typed? typed-type typed-value
    typed-with-value
    typed-dynamic?
    typed-list->dynamic-values
    typed-map-value)
  (import
    (micascheme)
    (tico type))

  (data (typed type value))

  (define (typed-with-value $typed $value)
    (typed (typed-type $typed) $value))

  (define (typed-map-value $typed $fn)
    (lets
      ($type (typed-type $typed))
      (typed-with-value $typed
        (and (type-dynamic? $type)
          (app $fn (typed-value $typed))))))

  (define (typed-dynamic? $typed)
    (and 
      (type-dynamic? (typed-type $typed)) 
      $typed))

  (define (typed-list->dynamic-values $typed-list)
    (map typed-value (filter typed-dynamic? $typed-list)))
)
