(library (micalang reify)
  (export
    unique-id
    default-ids-reify
    ids-reify
    reify
    check-reify)
  (import
    (micalang base)
    (micalang term))

  (define (unique-id $ids $symbol?)
    (cond
      ((not $symbol?)
        (unique-id $ids 'x))
      ((member $symbol? $ids)
        (unique-id $ids (symbol-append '$ $symbol?)))
      (else
        $symbol?)))

  (define (reify $term)
    (ids-reify (stack) $term))

  (define (ids-reify $ids $term)
    (default-ids-reify
      (lambda ($default $ids $term) (throw reify $term))
      $ids
      $term))

  (define (default-ids-reify $default $ids $term)
    (switch $term
      ((a-type? _) 'a-type)
      ((a-symbol? _) 'a-symbol)
      ((a-boolean? _) 'a-boolean)
      ((a-number? _) 'a-number)
      ((a-char? _) 'a-char)
      ((a-string? _) 'a-string)
      ((native? $native)
        (native-ref $native))
      ((variable? $variable)
        (list-ref $ids (variable-index $variable)))
      ((constant? $constant)
        (constant-ref $constant))
      ((tagged? $tagged)
        ; TODO: Allow chaining like application.
        `(
          ,(default-ids-reify $default $ids (tagged-tag $tagged))
          ,(default-ids-reify $default $ids (tagged-ref $tagged))))
      ((abstraction? $abstraction)
        (lets
          ($symbol? (abstraction-symbol? $abstraction))
          ($id (unique-id $ids $symbol?))
          ($reified-param (default-ids-reify $default $ids (abstraction-param $abstraction)))
          `(lambda
            ,(if $symbol? `(,$id ,$reified-param) $reified-param) .
            ,(lets
              ($body (abstraction-apply $abstraction (native $id)))
              ($ids (push $ids $id))
              ($reified-body (default-ids-reify $default $ids $body))
              (if (abstraction? $body)
                (cdr $reified-body)
                `(,$reified-body))))))
      ((type-abstraction? $type-abstraction)
        (lets
          ($symbol? (type-abstraction-symbol? $type-abstraction))
          ($id (unique-id $ids $symbol?))
          ($reified-param (default-ids-reify $default $ids (type-abstraction-param $type-abstraction)))
          `(a-lambda
            ,(if $symbol? `(,$id ,$reified-param) $reified-param) .
            ,(lets
              ($body (type-abstraction-apply $type-abstraction (native $id)))
              ($ids (push $ids $id))
              ($reified-body (default-ids-reify $default $ids $body))
              (if (type-abstraction? $body)
                (cdr $reified-body)
                `(,$reified-body))))))
      ((application? $application)
        (lets
          ($lhs (application-lhs $application))
          ($reified-lhs (default-ids-reify $default $ids (application-lhs $application)))
          ($reified-rhs (default-ids-reify $default $ids (application-rhs $application)))
          (if (application? $lhs)
            (append $reified-lhs `(,$reified-rhs))
            `(,$reified-lhs ,$reified-rhs))))
      ((conditional? $conditional)
        `(if
          ,(default-ids-reify $default $ids (conditional-cond $conditional))
          ,(default-ids-reify $default $ids (conditional-true $conditional))
          ,(default-ids-reify $default $ids (conditional-false $conditional))))
      ((macro? $macro)
        'macro)
      ((else $other)
        ($default $default $other))))

  (define-rule-syntax (check-reify in out)
    (check (equal? (ids-reify '(v0 v1) in) `out)))
)
