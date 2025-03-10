(library (typed compiler)
  (export
    compiler compiler? compiler-evaluator compiler-lookup
    constant constant? constant-value
    dynamic dynamic? dynamic-value)
  (import (micascheme))

  (data (constant value))
  (data (dynamic value))
  (data (compiler evaluator lookup))

  (define (compiler+ $compiler $identifier $typed)
    (compiler
      (switch-exhaustive (typed-value $typed)
        ((constant? $constant)
          (evaluator-push
            (compiler-evaluator $compiler)
            (cons
              (syntax->datum $identifier)
              (constant-value $constant))))
        ((dynamic? $dynamic)
          $evaluator))
      (lookup+undefined
        (compiler-lookup $compiler)
        $identifier
        $typed)))
)
