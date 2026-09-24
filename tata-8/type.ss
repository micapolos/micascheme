(library (tata-8 type)
  (export
    integer-type
    integer-type?

    text-type
    text-type?

    drawing-type
    drawing-type?

    type?
    type-switch)
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
  (data text-type)
  (data drawing-type)
  (union (type integer-type text-type drawing-type))

  (define-rule-syntax (tata-string x)
    (code-string (app (typed-ref (typed-code x)) '())))
)
