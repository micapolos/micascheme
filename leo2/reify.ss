(library (leo2 reify)
  (export
    reify
    check-reify)
  (import
    (leo2 base)
    (leo2 term)
    (symbol)
    (leo2 symbol))

  (define (reify $depth $term)
    (switch-exhaustive $term
      ((type? $type)
        (case (type-depth $type)
          ((0) `a-type)
          (else
            (symbol-append
              (reify $depth (type (- (type-depth $type) 1)))
              `-
              `type))))
      ((variable? $variable)
        (variable-symbol $variable))
      ((native? $native)
        (native-ref $native))
      ((native-application? $native-application)
        `(native-apply
          ,(reify $depth (native-application-target $native-application))
          ,@(map
            (partial reify $depth)
            (native-application-args $native-application))))
      ((abstraction? $abstraction)
        (lets
          ($symbol (depth->symbol $depth))
          ($body (abstraction-apply $abstraction (variable $symbol)))
          `(lambda ,$symbol
            ,@(app
              (if (abstraction? $body) cdr list)
              (reify (+ $depth 1) $body)))))
      ((recursive? $recursive)
        (lets
          ($symbol (depth->symbol $depth))
          ($body (recursive-apply $recursive (variable $symbol)))
          `(recursive lambda ,$symbol
            ,@(app
              (if (abstraction? $body) cdr list)
              (reify (+ $depth 1) $body)))))
      ((abstraction-type? $abstraction-type)
        (lets
          ($symbol (depth->symbol $depth))
          ($body (abstraction-type-apply $abstraction-type (variable $symbol)))
          `(a-lambda
            (,$symbol :
              ,(reify $depth (abstraction-type-param $abstraction-type)))
            ,@(app
              (if (abstraction-type? $body) cdr list)
              (reify (+ $depth 1) $body)))))
      ((application? $application)
        (lets
          ($lhs (application-lhs $application))
          (append
            (app
              (if (application? $lhs) identity list)
              (reify $depth $lhs))
            (list (reify $depth (application-rhs $application))))))
      ((branch? $branch)
        `(if
          ,(reify $depth (branch-condition $branch))
          ,(reify $depth (branch-consequent $branch))
          ,(reify $depth (branch-alternate $branch))))))

  (define-rule-syntax (check-reify in out)
    (check (equal? (reify 0 in) 'out)))
)
