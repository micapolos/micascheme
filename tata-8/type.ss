(library (tata-8 type)
  (export
    integer-type
    integer-type?

    drawing-type
    drawing-type?

    type?)
  (import
    (scheme)
    (code)
    (data)
    (union)
    (keyword)
    (procedure)
    (lets)
    (check)
    (syntax))

  (data integer-type)
  (data drawing-type)
  (union (type integer-type drawing-type))

  (define-rule-syntax (tata-string x)
    (code-string (app (typed-ref (typed-code x)) '())))
)
