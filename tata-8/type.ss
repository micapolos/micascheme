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
    type-switch

    type->datum

    symbolic-type-index-of)
  (import
    (scheme)
    (code)
    (data)
    (union)
    (keyword)
    (procedure)
    (lets)
    (check)
    (throw)
    (list)
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

  (define (type->datum $type)
    (type-switch $type
      ((integer-type? _) 'integer)
      ((text-type? _) 'text)
      ((image-type? _) 'image)
      ((drawing-type? _) 'drawing)
      ((game-type? _) 'game)
      ((symbolic-type? $symbolic-type)
        `(
          ,(symbolic-type-symbol $symbolic-type)
          ,@(map type->datum
            (symbolic-type-args $symbolic-type))))))

  (define (symbolic-type-index-of $symbolic-type $arg-type)
    (or
      (find-index (partial equal? $arg-type) (symbolic-type-args $symbolic-type))
      (throw (not found))))
)
