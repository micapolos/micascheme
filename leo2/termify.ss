(library (leo2 termify)
  (export termify)
  (import
    (leo2 base)
    (leo2 term))

  (define (termify $type $value)
    (term-switch $type
      ((evaluated? $evaluated)
        (termify (evaluated-ref $evaluated $value)))
      ((type? $type)
        'erased)
      ((typed? $typed)
        (type-termify
          (typed-type $typed)
          (typed-ref $typed)
          $value))))
)
