(library (leo2 datum)
  (export term->datum)
  (import
    (leo2 base)
    (leo2 term)
    (leo2 symbol))

  (define (term->datum $depth $strip-evaluated? $term)
    (switch $term
      ((evaluated? $evaluated)
        (if $strip-evaluated?
          (term->datum $depth $strip-evaluated?
            (evaluated-ref $evaluated))
          `(evaluated
            ,(term->datum $depth $strip-evaluated?
              (evaluated-ref $evaluated)))))
      ((variable? $variable)
        (variable-symbol $variable))
      ((type? $type)
        `(type ,(type-depth $type)))
      ((native? $native)
        `(native ,(native-ref $native)))
      ((native-application? $native-application)
        `(native-application
          ,(native-application-target $native-application)
          (list
            ,@(map
              (partial term->datum $depth $strip-evaluated?)
              (native-application-args $native-application)))))
      ((abstraction? $abstraction)
        `(abstraction
          ,(procedure->datum $depth $strip-evaluated?
            (abstraction-procedure $abstraction))))
      ((abstraction-type? $abstraction-type)
        `(abstraction-type
          ,(term->datum $depth $strip-evaluated?
            (abstraction-type-param $abstraction-type))
          ,(procedure->datum $depth $strip-evaluated?
            (abstraction-type-procedure $abstraction-type))))
      ((application? $application)
        `(application
          ,(term->datum $depth $strip-evaluated? (application-lhs $application))
          ,(term->datum $depth $strip-evaluated? (application-rhs $application))))
      ((recursion? $recursion)
        `(recursion
          ,(procedure->datum $depth $strip-evaluated?
            (recursion-procedure $recursion))))
      ((branch? $branch)
        `(branch
          ,(term->datum $depth $strip-evaluated? (branch-condition $branch))
          ,(term->datum $depth $strip-evaluated? (branch-consequent $branch))
          ,(term->datum $depth $strip-evaluated? (branch-alternate $branch))))
      ((else $other)
        $other)))

  (define (procedure->datum $depth $strip-evaluated? $procedure)
    (lets
      ($symbol (depth->symbol $depth))
      `(lambda (,$symbol)
        ,(term->datum
          (+ $depth 1)
          $strip-evaluated?
          (app $procedure (variable $symbol))))))
)
