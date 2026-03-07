(library (leo2 solver)
  (export
    empty-solutions
    solutions-push

    solver
    solver-with
    solver-lets
    solver-apply
    solutions-solver
    set-solutions-solver
    solver->datum
    list->solver
    apply-solver
    term-solver
    check-solver=?)
  (import
    (leo2 base)
    (leo2 term)
    (leo2 datum)
    (leo2 equal)
    (monadic))

  (define empty-solutions '())

  (define (solutions-hole-index? $solutions $hole)
    (lets
      ($index (- (length $solutions) (hole-index $hole) 1))
      (and (not (negative? $index)) $index)))

  (define (solutions-push $solutions $term)
    (values
      (push $solutions $term)
      (hole (length $solutions))))

  (define (solver-apply $solver $solutions)
    ($solver $solutions))

  (define (solver-solve $solver)
    (solver-apply $solver '()))

  (define (solver-bind $solver $fn)
    (solver ($solutions)
      (lets
        ((values $solutions $value) (solver-apply $solver $solutions))
        (solver-apply ($fn $value) $solutions))))

  (define-monadic solver)

  (define-rules-syntaxes
    ((solver ($solutions) body)
      (identifier? #'$solutions)
      (lambda ($solutions) body))
    ((solver $result)
      (solver ($solutions)
        (values $solutions $result)))
    ((solver-with $solutions $result)
      (solver (_)
        (values $solutions $result))))

  (define (solver->datum $solver)
    (lets
      ((values $solutions $result) (solver-solve $solver))
      `(solver
        (stack ,@(map term->datum (reverse $solutions)))
        ,(term->datum $result))))

  (define solutions-solver
    (solver ($solutions)
      (values $solutions $solutions)))

  (define (set-solutions-solver $solutions $solver)
    (solver (_)
      (solver-apply $solver $solutions)))

  (define (term-solver $depth $expected $actual)
    (switch $actual
      ((hole? $actual-hole)
        (term-solver $depth $actual $expected))
      ((else $actual-other)
        (non-hole-term-solver $depth $expected $actual-other))))

  (define (non-hole-term-solver $depth $expected $actual)
    (or
      (switch? $expected
        ((evaluated? $expected-evaluated)
          (switch? $actual
            ((evaluated? $actual-evaluated)
              (apply-solver evaluated
                (term-solver $depth
                  (evaluated-ref $expected-evaluated)
                  (evaluated-ref $actual-evaluated))))))
        ((typed? $expected-typed)
          (switch? $actual
            ((typed? $actual-typed)
              (apply-solver typed
                (term-solver $depth
                  (typed-type $expected-typed)
                  (typed-type $actual-typed))
                (term-solver $depth
                  (typed-ref $expected-typed)
                  (typed-ref $actual-typed))))))
        ((hole? $hole)
          (solver-lets
            ($solutions solutions-solver)
            (switch (solutions-hole-index? $solutions $hole)
              ((false? _)
                (solver (unbound $hole)))
              ((else $index)
                (switch (list-ref $solutions $index)
                  ((unknown? _)
                    (solver-with
                      (list-set $solutions $index $actual)
                      $actual))
                  ((else $other)
                    (term-solver $depth $other $actual)))))))
        ((variable? $expected-variable)
          (switch? $actual
            ((variable? $actual-variable)
              (and
                (=
                  (variable-index $expected-variable)
                  (variable-index $actual-variable))
                (solver $actual-variable)))))
        ((unknown? _)
          (solver $actual))
        ((native-type? _)
          (switch? $actual
            ((native-type? $native-type)
              (solver $native-type))))
        ((native? $expected-native)
          (switch? $actual
            ((native? $actual-native)
              (and
                (equal?
                  (native-ref $expected-native)
                  (native-ref $actual-native))
                (solver $actual-native)))))
        ((type? $expected-type)
          (switch? $actual
            ((type? $actual-type)
              (and
                (=
                  (type-depth $expected-type)
                  (type-depth $actual-type))
                (solver $actual-type)))))
        ((lambda? $expected-lambda)
          (switch? $actual
            ((lambda? $actual-lambda)
              (solver-lets
                ($body
                  (term-solver
                    (+ $depth 1)
                    ($expected-lambda (variable $depth))
                    ($actual-lambda (variable $depth))))
                (solver (lambda ($0) $body))))))
        ((lambda-type? $expected-lambda-type)
          (switch? $actual
            ((lambda-type? $actual-lambda-type)
              (apply-solver lambda-type
                (term-solver $depth
                  (lambda-type-param $expected-lambda-type)
                  (lambda-type-param $actual-lambda-type))
                (term-solver $depth
                  (lambda-type-lambda $expected-lambda-type)
                  (lambda-type-lambda $actual-lambda-type))))))
        ((recursion? $expected-recursion)
          (switch? $actual
            ((recursion? $actual-recursion)
              (apply-solver recursion
                (term-solver $depth
                  (recursion-lambda $expected-recursion)
                  (recursion-lambda $actual-recursion))))))
        ((application? $expected-application)
          (switch $actual
            ((application? $actual-application)
              (apply-solver application
                (term-solver $depth
                  (application-lhs $expected-application)
                  (application-lhs $actual-application))
                (term-solver $depth
                  (application-rhs $expected-application)
                  (application-rhs $actual-application)))))))
      (solver (mismatch $expected $actual))))

  (define-rule-syntax (check-solver=? in out)
    (check
      (equal?
        (solver->datum in)
        (solver->datum out))))
)
