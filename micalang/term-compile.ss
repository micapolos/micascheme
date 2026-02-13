(library (micalang term-compile)
  (export
    term-compile
    default-term-compile)
  (import
    (micalang base)
    (micalang term))

  (define (term-compile $term)
    (default-term-compile
      (lambda ($default $term) (throw term-compile))
      $term))

  (define (default-term-compile $default $term)
    (switch $term
      ((a-type? $type)
        (throw erased))
      ((native? $native)
        ($default $default (native-ref $native)))
      ((variable? $variable)
        (variable-symbol $variable))
      ((constant? $constant)
        (default-term-compile $default (constant-ref $constant)))
      ((tagged? $tagged)
        (default-term-compile $default (tagged-ref $tagged)))
      ((abstraction? $abstraction)
        (lets
          ($symbol (or (abstraction-symbol? $abstraction) '_))
          `(lambda (,$symbol)
            ,(default-term-compile $default
              (abstraction-apply $abstraction (variable $symbol))))))
      ((type-abstraction? $type-abstraction)
        (throw erased))
      ((application? $application)
        `(
          ,(default-term-compile $default (application-lhs $application))
          ,(default-term-compile $default (application-rhs $application))))
      ((conditional? $conditional)
        `(if
          ,(default-term-compile $default (conditional-cond $conditional))
          ,(default-term-compile $default (conditional-true $conditional))
          ,(default-term-compile $default (conditional-false $conditional))))
      ((else $other)
        ($default $default $other))))
)
