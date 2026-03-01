(library (leo2 solver)
  (export
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
    (leo2 equal))

  (define (solver-apply $solver $solutions)
    ($solver $solutions))

  (define (solver-solve $solver)
    (solver-apply $solver '()))

  (define-rules-syntaxes
    ((solver ($solutions) body)
      (identifier? #'$solutions)
      (lambda ($solutions) body))
    ((solver $result)
      (solver ($solutions)
        (values $solutions $result)))
    ((solver-with $solutions $result)
      (solver (_)
        (values $solutions $result)))
    ((solver-lets solver)
      solver)
    ((solver-lets (id first-solver) x ... last-solver)
      (identifier? #'id)
      (solver ($solutions)
        (lets
          ((values $solutions id)
            (solver-apply first-solver $solutions))
          (solver-apply
            (solver-lets x ...
              (switch id
                ((nothing? $nothing) (solver $nothing))
                ((else _) last-solver)))
            $solutions)))))

  (define (solver->datum $solver)
    (lets
      ((values $solutions $result) (solver-solve $solver))
      `(solver
        (stack ,@(map term->datum (reverse $solutions)))
        ,(term->datum $result))))

  (define (list->solver $solvers)
    (switch-exhaustive $solvers
      ((null? $null)
        (solver $null))
      ((pair? $pair)
        (apply-solver cons
          (car $pair)
          (list->solver (cdr $pair))))))

  (define-case-syntax (apply-solver fn arg ...)
    (lets
      ($args #'(arg ...))
      ($tmps (generate-temporaries $args))
      #`(solver-lets
        #,@(map-with
          ($tmp $tmps)
          ($arg $args)
          #`(#,$tmp #,$arg))
        (solver (fn #,@$tmps)))))

  (define solutions-solver
    (solver ($solutions)
      (values $solutions $solutions)))

  (define (set-solutions-solver $solutions $solver)
    (solver (_)
      (solver-apply $solver $solutions)))

  (define (solutions-index $solutions $hole)
    (- (length $solutions) (hole-index $hole) 1))

  (define (solutions-ref-solver $hole)
    (solver-lets
      ($solutions solutions-solver)
      (switch (list-ref? $solutions (solutions-index $solutions $hole))
        ((false? _) (solver nothing))
        ((else $term) (solver $term)))))

  (define (solutions-set-solver $hole $term)
    (solver-lets
      ($solutions solutions-solver)
      (solver-with
        (list-set $solutions (solutions-index $solutions $hole) $term)
        $term)))

  (define (term-solver $depth $expected $actual)
    (switch $actual
      ((hole? $actual-hole)
        (term-solver $depth $actual $expected))
      ((else $actual-other)
        (non-hole-term-solver $depth $expected $actual-other))))

  (define (non-hole-term-solver $depth $expected $actual)
    (or
      (switch $expected
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
            ($solution (solutions-ref-solver $hole))
            (switch $solution
              ((nothing? $nothing)
                (solver $nothing))
              ((unknown? _)
                (solutions-set-solver $hole $actual))
              ((else $other)
                (term-solver $depth $other $actual)))))
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
        ; TODO: Cover all term types, and don't use term=?
        ((else _)
          (and
            (term=? $expected $actual)
            (solver $actual))))
      (solver nothing)))

  (define-rule-syntax (check-solver=? in out)
    (check
      (equal?
        (solver->datum in)
        (solver->datum out))))
)
