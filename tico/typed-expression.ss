(library (tico typed-expression)
  (export
    structd-expression)
  (import
    (micascheme)
    (tico type)
    (tico typed)
    (tico expression))

  (define (structd-expression $symbol $typed-expressions)
    (typed
      (struct $symbol
        (map typed-type $typed-expressions))
      (tuple-expression
        (typed-list->dynamic-values $typed-expressions))))
)
