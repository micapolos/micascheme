(library (tata-8 type)
  (export
    integer-type
    integer-type?

    text-type
    text-type?

    image-type
    image-type?

    drawing-type
    drawing-type?

    game-type
    game-type?

    symbolic-type
    symbolic-type?
    symbolic-type-symbol
    symbolic-type-args

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
  (data image-type)
  (data drawing-type)
  (data game-type)
  (data (symbolic-type symbol args))
  (union
    (type
      integer-type
      text-type
      image-type
      drawing-type
      game-type
      symbolic-type))

  (define-rule-syntax (tata-string x)
    (code-string (app (typed-ref (typed-code x)) '())))
)
