(library (leo2 runtime)
  (export
    type
    native native-apply
    indexed symbolic symbol literal
    variable
    lambda a-lambda
    apply recursion
    if
    annotated
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
      (recursion %recursion)
      (annotated %annotated)))
  (export
    (import
      (only (leo2 base) quote)))

  (define-rules-syntaxes
    ((type n) #f)
    ((typed _ v) v)
    ((symbol x) #f)
    ((literal x) x)
    ((indexed index x) x)
    ((symbolic symbol x) x)
    ((native _ x) x)
    ((native-apply _ fn arg ...) (fn arg ...))
    ((variable _ x) x)
    ((lambda (id _) body) (%lambda (id) body))
    ((a-lambda (id t) body) #f)
    ((apply lhs rhs) (lhs rhs))
    ((recursion id _ body) (letrec ((id body)) id))
    ((if a b c) (%if a b c))
    ((annotated _ x) x)
    ((check=? in out) (check (equal? in out))))
)
