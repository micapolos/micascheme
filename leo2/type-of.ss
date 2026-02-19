(library (leo2 type-of)
  (export type-of)
  (import
    (leo2 base)
    (leo2 term))

  (define (type-of $term)
    (switch-exhaustive $term
      ((type? $type)
        (type (+ (type-depth $type) 1)))
      ((typed? $type)
        (typed-type $type))))
)
