(library (micalang reify)
  (export default-reify reify check-reify)
  (import
    (micalang base)
    (micalang term))

  (define (index->symbol $index)
    (string->symbol (format "v~a" $index)))

  (define (reify $term)
    (default-reify
      (lambda ($default $term) (throw reify $term))
      $term))

  (define (default-reify $default $term)
    (switch $term
      ((a-type? _) 'a-type)
      ((native? $native)
        (native-ref $native))
      ((variable? $variable)
        (variable-symbol $variable))
      ((constant? $constant)
        (constant-ref $constant))
      ((tagged? $tagged)
        ; TODO: Allow chaining like application.
        `(
          ,(default-reify $default (tagged-tag $tagged))
          ,(default-reify $default (tagged-ref $tagged))))
      ((abstraction? $abstraction)
        (lets
          ($symbol? (abstraction-symbol? $abstraction))
          ($reified-param (default-reify $default (abstraction-param $abstraction)))
          `(lambda
            ,(if $symbol? `(,$symbol? ,$reified-param) $reified-param) .
            ,(lets
              ($body (abstraction-apply $abstraction (native $symbol?)))
              ($reified-body (default-reify $default $body))
              (if (abstraction? $body)
                (cdr $reified-body)
                `(,$reified-body))))))
      ((application? $application)
        (lets
          ($lhs (application-lhs $application))
          ($reified-lhs (default-reify $default (application-lhs $application)))
          ($reified-rhs (default-reify $default (application-rhs $application)))
          (if (application? $lhs)
            (append $reified-lhs `(,$reified-rhs))
            `(,$reified-lhs ,$reified-rhs))))
      ((conditional? $conditional)
        `(if
          ,(default-reify $default (conditional-cond $conditional))
          ,(default-reify $default (conditional-true $conditional))
          ,(default-reify $default (conditional-false $conditional))))
      ((type-abstraction? $type-abstraction)
        (lets
          ($symbol? (type-abstraction-symbol? $type-abstraction))
          ($reified-param (default-reify $default (type-abstraction-param $type-abstraction)))
          `(a-lambda
            ,(if $symbol? `(,$symbol? ,$reified-param) $reified-param) .
            ,(lets
              ($body (type-abstraction-apply $type-abstraction (native $symbol?)))
              ($reified-body (default-reify $default $body))
              (if (type-abstraction? $body)
                (cdr $reified-body)
                `(,$reified-body))))))
      ((macro? $macro)
        'macro)
      ((else $other)
        ($default $default $other))))

  (define-rule-syntax (check-reify in out)
    (check (equal? (reify in) `out)))
)
