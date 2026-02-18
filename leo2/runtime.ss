(library (leo2 runtime)
  (export
    a-type
    native native-apply
    variable
    lambda a-lambda
    apply recursive
    if
    check=?)
  (import
    (leo2 datum)
    (rename (leo2 base)
      (lambda %lambda)
      (apply %apply)
      (if %if))
    (rename (leo2 term)
      (native %native)
      (variable %variable)
      (recursive %recursive)))

  (define a-type 'erased)

  (define-rule-syntax (native x) x)

  (define-rule-syntax (native-apply fn arg ...)
    (fn arg ...))

  (define-rule-syntax (variable x) x)

  (define-rule-syntax (lambda id body)
    (%lambda (id) body))

  (define-rules-syntax
    ((a-lambda (id t) body) 'erased))

  (define-rule-syntax (apply lhs rhs)
    (lhs rhs))

  (define-rule-syntax (recursive id body)
    (letrec ((id body)) id))

  (define-rule-syntax (if a b c)
    (%if a b c))

  (define-rule-syntax (check=? in out)
    (check (equal? in out)))
)
