(library (leo2 runtime)
  (export
    type
    native native-apply
    indexed symbolic symbol literal
    variable
    lambda a-lambda
    apply recursion
    if
    check=?)
  (import
    (leo2 datum)
    (rename (leo2 base)
      (lambda %lambda)
      (apply %apply)
      (if %if))
    (rename (leo2 term)
      (indexed %indexed)
      (symbolic %symbolic)
      (type %type)
      (typed %typed)
      (native %native)
      (variable %variable)
      (recursion %recursion)))
  (export
    (import
      (only (leo2 base) quote)))

  (define-rule-syntax (type n) #f)

  (define-rule-syntax (typed _ v) v)

  (define-rule-syntax (symbol x) #f)

  (define-rule-syntax (literal x) x)

  (define-rule-syntax (indexed index x) x)

  (define-rule-syntax (symbolic symbol x) x)

  (define-rule-syntax (native _ x) x)

  (define-rule-syntax (native-apply _ fn arg ...)
    (fn arg ...))

  (define-rule-syntax (variable _ x) x)

  (define-rule-syntax (lambda (id _) body)
    (%lambda (id) body))

  (define-rules-syntax
    ((a-lambda (id t) body) #f))

  (define-rule-syntax (apply lhs rhs)
    (lhs rhs))

  (define-rule-syntax (recursion id _ body)
    (letrec ((id body)) id))

  (define-rule-syntax (if a b c)
    (%if a b c))

  (define-rule-syntax (check=? in out)
    (check (equal? in out)))
)
