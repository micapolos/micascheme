(library (micalang reify)
  (export reify check-reify)
  (import
    (micalang base)
    (micalang term))

  (define (index->symbol $index)
    (string->symbol (format "v~a" $index)))

  (define (reify $term)
    (depth-reify 0 $term))

  (define (depth-reify $depth $term)
    (switch-exhaustive $term
      ((native? $native)
        (native-ref $native))
      ((abstraction? $abstraction)
        (lets
          ($symbol (abstraction-symbol $abstraction))
          `(lambda (,$symbol type) .
            ,(lets
              ($body (abstraction-apply $abstraction (native $symbol)))
              ($reified-body (depth-reify (+ $depth 1) $body))
              (if (abstraction? $body)
                (cdr $reified-body)
                `(,$reified-body))))))
      ((application? $application)
        (lets
          ($lhs (application-lhs $application))
          ($reified-lhs (depth-reify $depth (application-lhs $application)))
          ($reified-rhs (depth-reify $depth (application-rhs $application)))
          (if (application? $lhs)
            (append $reified-lhs `(,$reified-rhs))
            `(,$reified-lhs ,$reified-rhs))))
      ((conditional? $conditional)
        `(if
          ,(depth-reify $depth (conditional-cond $conditional))
          ,(depth-reify $depth (conditional-true $conditional))
          ,(depth-reify $depth (conditional-false $conditional))))
      ((pi? $pi)
        (lets
          ($symbol? (pi-symbol? $pi))
          ($reified-param (depth-reify $depth (pi-param $pi)))
          `(pi
            ,(if $symbol? `(,$symbol? ,$reified-param) $reified-param) .
            ,(lets
              ($body (pi-apply $pi (native $symbol?)))
              ($reified-body (depth-reify (+ $depth 1) $body))
              (if (pi? $body)
                (cdr $reified-body)
                `(,$reified-body))))))))

  (define-rule-syntax (check-reify in out)
    (check (equal? (depth-reify 0 in) `out)))
)
