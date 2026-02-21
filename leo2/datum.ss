(library (leo2 datum)
  (export
    term->datum
    check-term->datum=?)
  (import
    (leo2 base)
    (leo2 term)
    (leo2 symbol))

  (define (term->datum $strip-typed? $strip-evaluated? $depth $term)
    (switch-exhaustive $term
      ((evaluated? $evaluated)
        (if $strip-evaluated?
          (term->datum $strip-typed? $strip-evaluated? $depth
            (evaluated-ref $evaluated))
          `(evaluated
            ,(term->datum $strip-typed? $strip-evaluated? $depth
              (evaluated-ref $evaluated)))))
      ((type? $type)
        `(type ,(type-depth $type)))
      ((typed? $typed)
        (lets
          ($datum
            (type-term->datum $strip-typed? $strip-evaluated? $depth
              (typed-type $typed)
              (typed-ref $typed)))
          (if $strip-typed?
            $datum
            `(typed
              ,(term->datum $strip-typed? $strip-evaluated? $depth
                (typed-type $typed))
              ,$datum))))))

  (define (type-term->datum $strip-typed? $strip-evaluated? $depth $type $term)
    (switch $term
      ((variable? $variable)
        (variable-symbol $variable))
      ((type? $type)
        `(type ,(type-depth $type)))
      ((native? $native)
        `(native
          ,(term->datum $strip-typed? $strip-evaluated? $depth $type)
          ,(native-ref $native)))
      ((native-application? $native-application)
        `(native-apply
          ,(term->datum $strip-typed? $strip-evaluated? $depth $type)
          ,(native-application-procedure $native-application)
          (list
            ,@(map
              (partial term->datum $strip-typed? $strip-evaluated? $depth)
              (native-application-args $native-application)))))
      ((abstraction? $abstraction)
        `(lambda
          ,(procedure->datum $strip-typed? $strip-evaluated? $depth
            (abstraction-procedure $abstraction)
            (abstraction-type-param (typed-ref $type)))))
      ((abstraction-type? $abstraction-type)
        `(a-lambda
          ,(procedure->datum $strip-typed? $strip-evaluated? $depth
            (abstraction-type-procedure $abstraction-type)
            (abstraction-type-param $abstraction-type))))
      ((application? $application)
        `(apply
          ,(term->datum $strip-typed? $strip-evaluated? $depth (application-lhs $application))
          ,(term->datum $strip-typed? $strip-evaluated? $depth (application-rhs $application))))
      ((recursion? $recursion)
        `(recursive
          ,(procedure->datum $strip-typed? $strip-evaluated? $depth
            (recursion-procedure $recursion)
            (abstraction-type-param (typed-ref $type)))))
      ((branch? $branch)
        `(if
          ,(term->datum $strip-typed? $strip-evaluated? $depth (branch-condition $branch))
          ,(term->datum $strip-typed? $strip-evaluated? $depth (branch-consequent $branch))
          ,(term->datum $strip-typed? $strip-evaluated? $depth (branch-alternate $branch))))
      ((symbolic? $symbolic)
        `(symbolic
          ,(symbolic-symbol $symbolic)
          ,(term->datum $strip-typed? $strip-evaluated? $depth (symbolic-ref $symbolic))))
      ((symbol? $symbol)
        $symbol)
      ((else $other)
        $other)))

  (define (procedure->datum $strip-datum? $strip-evaluated? $depth $procedure $type)
    (lets
      ($symbol (depth->symbol $depth))
      `(lambda (,$symbol ,(term->datum $strip-datum? $strip-evaluated? $depth $type))
        ,(term->datum
          $strip-datum? $strip-evaluated?
          (+ $depth 1)
          (app $procedure (typed $type (variable $symbol)))))))

  (define-rule-syntax (check-term->datum=? in out)
    (check (equal? (term->datum #t #t 0 in) 'out)))
)
