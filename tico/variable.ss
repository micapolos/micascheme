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

  (define (variable-promote (variable $index) $arity)
    (lets
      ($index (- $index $arity))
      (and (>= $index 0) (variable $index))))

  (define (variable-index-flatten $indices)
    (apply max $indices))

  (define (variable-index+ $first $second)
    (max $first $second))

  (define (variable+ (variable $first-index) (variable $second-index))
    (variable
      (variable-index+
        $first-index
        $second-index)))

  (define (variable-flatten (pair $first-variable $other-variables))
    (fold-left variable+ $first-variable $other-variables))
)
