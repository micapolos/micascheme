(library (tata-8 type)
  (export
    integer-type
    integer-type?

    string-type
    string-type?

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
  (data string-type)
  (data drawing-type)
  (union (type integer-type string-type drawing-type))

  (define-rule-syntax (tata-string x)
    (code-string (app (typed-ref (typed-code x)) '())))
)
