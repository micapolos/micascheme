(library (leo2 typed)
  (export
    term-type
    term-value
    typed-term)
  (import
    (leo2 base)
    (leo2 term))

  (define (term-type $term)
    (switch $term
      ((typed? $typed) (typed-type $term))
      ((type? $type) (type+1 $type))
      ((else $other) (throw term-type $other))))

  (define (term-value $term)
    (switch $term
      ((typed? $typed) (typed-ref $term))
      ((type? $type) $type)
      ((else $other) (throw term-value $other))))

  (define (typed-term $type $value)
    (switch $value
      ((type? $type) $type)
      ((else $other) (typed $type $other))))
)
