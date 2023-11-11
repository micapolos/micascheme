(library (tico variable)
  (export
    variable variable? variable-index
    variable-promote
    variable-index+
    variable-index-flatten
    variable+
    variable-flatten)
  (import
    (micascheme))

  (data (variable index))

  (define (variable-promote $variable $size)
    (lets
      ($index (- (variable-index $variable) $size))
      (and (>= $index 0)
        (variable $index))))

  (define (variable-index-flatten $indices)
    (apply max $indices))

  (define (variable-index+ $first $second)
    (max $first $second))

  (define (variable+ $first-variable $second-variable)
    (variable
      (variable-index+
        (variable-index $first-variable)
        (variable-index $second-variable))))

  (define (variable-flatten $variables)
    (fold-left variable+ (car $variables) (cdr $variables)))
)
