(library (leo2 datum)
  (export term->datum)
  (import
    (leo2 base)
    (leo2 term))

  (define (depth->symbol $depth)
    (string->symbol (format "v~a" $depth)))

  (define (term->datum $depth $term)
    (switch-exhaustive $term
      ((evaluated? $evaluated)
        `(evaluated
          ,(term->datum $depth (evaluated-ref $evaluated))))
      ((variable? $variable)
        (variable-symbol $variable))
      ((type? $type)
        `(type ,(type-depth $type)))
      ((native? $native)
        `(native ,(native-ref $native)))
      ((native-application? $native-application)
        `(native-application
          ,(native-application-procedure $native-application)
          (list
            ,@(map
              (partial term->datum $depth)
              (native-application-args $native-application)))))
      ((abstraction? $abstraction)
        `(abstraction
          ,(procedure->datum $depth
            (abstraction-procedure $abstraction))))
      ((abstraction-type? $abstraction-type)
        `(abstraction-type
          ,(term->datum $depth
            (abstraction-type-param $abstraction-type))
          ,(procedure->datum $depth
            (abstraction-type-procedure $abstraction-type))))
      ((application? $application)
        `(application
          ,(term->datum $depth (application-lhs $application))
          ,(term->datum $depth (application-rhs $application))))
      ((recursive? $recursive)
        `(recursive
          ,(procedure->datum $depth
            (recursive-procedure $recursive))))
      ((branch? $branch)
        `(branch
          ,(term->datum $depth (branch-condition $branch))
          ,(term->datum $depth (branch-consequent $branch))
          ,(term->datum $depth (branch-alternate $branch))))))

  (define (procedure->datum $depth $procedure)
    (lets
      ($symbol (depth->symbol $depth))
      `(lambda (,$symbol)
        ,(term->datum
          (+ $depth 1)
          (app $procedure (variable $symbol))))))
)
