(library (tico typed-expression)
  (export
    struct-typed-expression)
  (import
    (micascheme)
    (tico type)
    (tico typed)
    (tico expression))

  (define (struct-typed-expression $symbol $typed-expressions)
    (typed
      (struct-type $symbol
        (map typed-type $typed-expressions))
      (tuple-expression
        (typed-list->dynamic-values $typed-expressions))))
)
