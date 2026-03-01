(library (leo2 solver)
  (export
    (rename (solver! solver))
    solver-with
    solver-lets
    solver-apply
    solutions-solver
    solver->datum
    check-solver=?)
  (import
    (leo2 base)
    (leo2 term)
    (leo2 datum))

  (data (solver lambda))

  (define (solver-apply $solver $solutions)
    ((solver-lambda $solver) $solutions))

  (define (solver-solve $solver)
    (solver-apply $solver '()))

  (define-rules-syntaxes
    ((solver! ($solutions) body)
      (solver (lambda ($solutions) body)))
    ((solver! $result)
      (solver! ($solutions)
        (values $solutions $result)))
    ((solver-with $solutions $result)
      (solver! (_)
        (values $solutions $result)))
    ((solver-lets solver)
      solver)
    ((solver-lets (id first-solver) x ... last-solver)
      (lets
        ((values $solutions $result) (solver-apply first-solver $solutions))
        (solver-lets x ...
          (switch $result
            ((nothing? $nothing) (solver! $nothing))
            ((else _) last-solver))))))

  (define (solver->datum $solver)
    (lets
      ((values $solutions $result) (solver-apply $solver '()))
      `(solver
        (stack ,@(map term->datum (reverse $solutions)))
        ,(term->datum $result))))

  (define solutions-solver
    (solver! ($solutions)
      (values $solutions $solutions)))

  (define-rule-syntax (check-solver=? in out)
    (check
      (equal?
        (solver->datum in)
        (solver->datum out))))
)
