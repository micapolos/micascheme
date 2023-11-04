(library (tico variable)
  (export
    variable variable? variable-index variable-dependencies
    variable-lets-datums
    variable-promote
    variables-index)
  (import
    (micascheme)
    (tico dependency))

  (data (variable index dependencies))

  (define (variable-lets-datums $variable)
    (reverse
      (map dependency-lets-datum
        (variable-dependencies $variable))))

  (define (variable-promote $variable $size)
    (lets
      ($index (- (variable-index $variable) $size))
      (and (>= $index 0)
        (variable $index
          (variable-dependencies $variable)))))

  (define (variables-index $variables)
    (apply max (map variable-index $variables)))
)
