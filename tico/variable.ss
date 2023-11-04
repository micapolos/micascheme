(library (tico variable)
  (export
    variable variable? variable-index variable-dependencies
    variable-lets-datums
    variable-promote
    variable-index-flatten
    variable-flatten)
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

  (define (variable-index-flatten $indices)
    (apply max $indices))

  (define (variable-flatten $variables)
    (variable
      (variable-index-flatten
        (map variable-index $variables))
      (dependencies-flatten
        (map variable-dependencies $variables))))
)
