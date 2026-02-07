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
      ((native? $native) (native-ref $native))
      ((abstraction? $abstraction)
        (lets
          ($symbol (index->symbol $depth))
          `(lambda (,$symbol type)
            ,(depth-reify (+ $depth 1)
              ((abstraction-procedure $abstraction)
                (native $symbol))))))
      ((application? $application)
        `(
          ,(depth-reify $depth (application-lhs $application))
          ,(depth-reify $depth (application-rhs $application))))
      ((conditional? $conditional)
        `(if
          ,(depth-reify $depth (conditional-cond $conditional))
          ,(depth-reify $depth (conditional-true $conditional))
          ,(depth-reify $depth (conditional-false $conditional))))
      ((pi? $pi)
        (lets
          ($symbol (index->symbol $depth))
          `(pi (,$symbol ,(depth-reify $depth (pi-param $pi)))
            ,(depth-reify (+ $depth 1)
              ((pi-procedure $pi) (native $symbol))))))))

  (define-rule-syntax (check-reify in out)
    (check (equal? (depth-reify 0 in) `out)))
)
