(library (tico term)
  (export
    native-term native-term? native-term-arity native-term-body
    application-term application-term? application-term-arity application-term-target application-term-args
    abstraction-term abstraction-term? abstraction-term-arity abstraction-term-body
    variable-term variable-term? variable-term-index
    tuple-term tuple-term? tuple-term-items
    ref-term ref-term? ref-term-target ref-term-index
    values-term values-term? values-term-items

    term-argument-arity)
  (import
    (micascheme)
    (tico arity))

  (data (native-term arity body))
  (data (application-term arity target args))
  (data (abstraction-term arity body))
  (data (variable-term index))
  (data (tuple-term items))
  (data (ref-term target index))
  (data (values-term items))

  (define (term-argument-arity $term)
    (switch-exclusive $term
      ((native-term? $native-term)
        (native-term-arity $native-term))
      ((application-term? $application-term)
        (application-term-arity $application-term))
      ((values-term? $values-term)
        (apply +
          (map term-argument-arity
            (values-term-items $values-term))))
      ((abstraction-term? _) 1)
      ((variable-term? _) 1)
      ((tuple-term? _) 1)
      ((ref-term? _) 1)))
)
