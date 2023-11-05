(library (tico variable)
  (export
    variable variable? variable-index variable-dependencies
    test-variable
    variable-lets-datums
    variable-promote
    variable-index+
    variable-index-flatten
    variable+
    variable-flatten
    variable+dependencies)
  (import
    (micascheme)
    (tico dependency))

  (data (variable index dependencies))

  (define-syntax-rule (test-variable $index $dependency-name ...)
    (variable $index
      (stack
        (test-dependency $dependency-name) ...)))

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

  (define (variable-index+ $first $second)
    (max $first $second))

  (define (variable+ $first-variable $second-variable)
    (variable
      (variable-index+
        (variable-index $first-variable)
        (variable-index $second-variable))
      (dependencies+
        (variable-dependencies $first-variable)
        (variable-dependencies $second-variable))))

  (define (variable-flatten $variables)
    (fold-left variable+ (car $variables) (cdr $variables)))

  (define (variable+dependencies $variable $dependencies)
    (variable
      (variable-index $variable)
      (push-all
        (variable-dependencies $variable)
        $dependencies)))
)
