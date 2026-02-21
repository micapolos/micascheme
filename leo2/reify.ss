(library (leo2 reify)
  (export
    reify
    check-reify)
  (import
    (leo2 base)
    (leo2 term)
    (leo2 stdlib)
    (symbol)
    (leo2 symbol)
    (leo2 dependent))

  (define (reify $depth $term)
    (switch-exhaustive $term
      ((evaluated? $evaluated)
        (reify $depth (evaluated-ref $evaluated)))
      ((type? $type)
        (case (type-depth $type)
          ((0) `a-type)
          (else
            (symbol-append
              (reify $depth (type (- (type-depth $type) 1)))
              `-
              `type))))
      ((typed? $typed)
        (type-reify $depth
          (typed-type $typed)
          (typed-ref $typed)))))

  (define (type-reify $depth $type $term)
    (switch-exhaustive $term
      ((evaluated? $evaluated)
        (reify $depth (evaluated-ref $evaluated)))
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
          ,(reify $depth $type)
          ,(native-application-procedure $native-application)
          ,@(map
            (partial reify $depth)
            (native-application-args $native-application))))
      ((abstraction? $abstraction)
        (lets
          ($symbol (depth->symbol $depth))
          ($body
            (abstraction-apply $abstraction
              (variable-term
                (abstraction-type-param (typed-ref $type))
                $symbol)))
          `(lambda
            (
              ,(if (abstraction-dependent? $abstraction) $symbol '_)
              :
              ,(reify $depth (abstraction-type-param (typed-ref $type))))
            ,@(app
              (if (abstraction? (typed-ref $body)) cdr list)
              (reify (+ $depth 1) $body)))))
      ((recursion? $recursion)
        (lets
          ($symbol (depth->symbol $depth))
          ($body (recursion-apply $recursion (variable $symbol)))
          `(recursive lambda
            ,(if (recursion-dependent? $recursion) $symbol '_)
            ,@(app
              (if (abstraction? (typed-ref $body)) cdr list)
              (reify (+ $depth 1) $body)))))
      ((abstraction-type? $abstraction-type)
        (lets
          ($symbol (depth->symbol $depth))
          ($body
            (abstraction-type-apply $abstraction-type
              (variable-term (abstraction-type-param $abstraction-type) $symbol)))
          ($param (reify $depth (abstraction-type-param $abstraction-type)))
          `(a-lambda
            ,(if (abstraction-type-dependent? $abstraction-type)
              `(,$symbol : ,$param)
              $param)
            ,@(app
              (if (abstraction-type? (typed-ref $body)) cdr list)
              (reify (+ $depth 1) $body)))))
      ((binding? $binding)
        (lets
          ($symbol (depth->symbol $depth))
          ($body (binding-apply $binding (variable $symbol)))
          `(let
            (,$symbol ,(reify $depth (binding-ref $binding)))
            ,@(app
              (if (binding? (typed-ref $body)) cdr list)
              (reify (+ $depth 1) $body)))))
      ((application? $application)
        (lets
          ($lhs (application-lhs $application))
          (append
            (app
              (if (application? (typed-ref $lhs)) identity list)
              (reify $depth $lhs))
            (list (reify $depth (application-rhs $application))))))
      ((branch? $branch)
        `(if
          ,(reify $depth (branch-condition $branch))
          ,(reify $depth (branch-consequent $branch))
          ,(reify $depth (branch-alternate $branch))))
      ((symbolic? $symbolic)
        (lets
          ($term (symbolic-ref $symbolic))
          `(
            ,(symbolic-symbol $symbolic) .
            ,(app
              (if
                (and
                  (typed? $term)
                  (app (or? symbolic?) (typed-ref $term)))
                identity
                list)
              (reify $depth $term)))))
      ((symbol? $symbol)
        $symbol)))

  (define-rule-syntax (check-reify in out)
    (check (equal? (reify 0 in) `out)))
)
