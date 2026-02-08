(library (micalang reify)
  (export fallback-reify reify check-reify)
  (import
    (micalang base)
    (micalang term))

  (define (index->symbol $index)
    (string->symbol (format "v~a" $index)))

  (define (reify $term)
    (fallback-reify
      (lambda ($fallback $term)
        (throw 'dupa))
      $term))

  (define (fallback-reify $fallback $term)
    (switch $term
      ((type? _) 'type)
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
              ($reified-body (fallback-reify $fallback $body))
              (if (abstraction? $body)
                (cdr $reified-body)
                `(,$reified-body))))))
      ((application? $application)
        (lets
          ($lhs (application-lhs $application))
          ($reified-lhs (fallback-reify $fallback (application-lhs $application)))
          ($reified-rhs (fallback-reify $fallback (application-rhs $application)))
          (if (application? $lhs)
            (append $reified-lhs `(,$reified-rhs))
            `(,$reified-lhs ,$reified-rhs))))
      ((conditional? $conditional)
        `(if
          ,(fallback-reify $fallback (conditional-cond $conditional))
          ,(fallback-reify $fallback (conditional-true $conditional))
          ,(fallback-reify $fallback (conditional-false $conditional))))
      ((pi? $pi)
        (lets
          ($symbol? (pi-symbol? $pi))
          ($reified-param (fallback-reify $fallback (pi-param $pi)))
          `(pi
            ,(if $symbol? `(,$symbol? ,$reified-param) $reified-param) .
            ,(lets
              ($body (pi-apply $pi (native $symbol?)))
              ($reified-body (fallback-reify $fallback $body))
              (if (pi? $body)
                (cdr $reified-body)
                `(,$reified-body))))))
      ((else $other)
        ($fallback $fallback $other))))

  (define-rule-syntax (check-reify in out)
    (check (equal? (reify in) `out)))
)
