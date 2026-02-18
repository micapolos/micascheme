(library (leo2 compile)
  (export
    compile
    check-compiles)
  (import
    (leo2 base)
    (leo2 term)
    (leo2 symbol))

  (define (compile $depth $term)
    (switch-exhaustive $term
      ((native? $native)
        (native-ref $native))
      ((native-application? $native-application)
        `(
          ,(native-application-procedure $native-application)
          ,@(map
            (partial compile $depth)
            (native-application-args $native-application))))
      ((variable? $variable)
        (variable-symbol $variable))
      ((abstraction? $abstraction)
        (lets
          ($symbol (depth->symbol $depth))
          `(lambda (,$symbol)
            ,(compile
              (+ $depth 1)
              ((abstraction-procedure $abstraction) (variable $symbol))))))
      ((recursive? $recursive)
        (lets
          ($symbol (depth->symbol $depth))
          `(letrec
            ((
              ,$symbol
              ,(compile
                (+ $depth 1)
                ((recursive-procedure $recursive) (variable $symbol)))))
            ,$symbol)))
      ((application? $application)
        `(
          ,(compile $depth (application-lhs $application))
          ,(compile $depth (application-rhs $application))))
      ((branch? $branch)
        `(if
          ,(compile $depth (branch-condition $branch))
          ,(compile $depth (branch-consequent $branch))
          ,(compile $depth (branch-alternate $branch))))))

  (define-rule-syntax (check-compiles in out)
    (check (equal? (compile 0 in) 'out)))
)
