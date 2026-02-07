(library (micalang reify)
  (export reify check-reify)
  (import
    (micalang base)
    (micalang term))

  (define (index->symbol $index)
    (string->symbol (format "v~a" $index)))

  (define (reify $term)
    (switch-exhaustive $term
      ((native? $native)
        (native-ref $native))
      ((variable? $variable)
        (variable-symbol $variable))
      ((abstraction? $abstraction)
        (lets
          ($symbol (abstraction-symbol $abstraction))
          `(lambda (,$symbol type) .
            ,(lets
              ($body (abstraction-apply $abstraction (native $symbol)))
              ($reified-body (reify $body))
              (if (abstraction? $body)
                (cdr $reified-body)
                `(,$reified-body))))))
      ((application? $application)
        (lets
          ($lhs (application-lhs $application))
          ($reified-lhs (reify (application-lhs $application)))
          ($reified-rhs (reify  (application-rhs $application)))
          (if (application? $lhs)
            (append $reified-lhs `(,$reified-rhs))
            `(,$reified-lhs ,$reified-rhs))))
      ((conditional? $conditional)
        `(if
          ,(reify (conditional-cond $conditional))
          ,(reify (conditional-true $conditional))
          ,(reify (conditional-false $conditional))))
      ((pi? $pi)
        (lets
          ($symbol? (pi-symbol? $pi))
          ($reified-param (reify (pi-param $pi)))
          `(pi
            ,(if $symbol? `(,$symbol? ,$reified-param) $reified-param) .
            ,(lets
              ($body (pi-apply $pi (native $symbol?)))
              ($reified-body (reify $body))
              (if (pi? $body)
                (cdr $reified-body)
                `(,$reified-body))))))))

  (define-rule-syntax (check-reify in out)
    (check (equal? (reify in) `out)))
)
