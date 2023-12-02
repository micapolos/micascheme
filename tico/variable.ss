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

  (function (variable-promote (variable $index) $arity)
    (lets
      ($index (- $index $arity))
      (and (>= $index 0) (variable $index))))

  (function (variable-index-flatten $indices)
    (apply max $indices))

  (function (variable-index+ $first $second)
    (max $first $second))

  (function (variable+ (variable $first-index) (variable $second-index))
    (variable
      (variable-index+
        $first-index
        $second-index)))

  (function (variable-flatten (pair $first-variable $other-variables))
    (fold-left variable+ $first-variable $other-variables))
)
