(library (leo2 comptime)
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

  (define a-type (type 0))

  (define-rule-syntax (native x)
    (%native x))

  (define-rule-syntax (native-apply fn arg ...)
    (native-application fn (list arg ...)))

  (define-rule-syntax (variable x)
    (%variable 'x))

  (define-rule-syntax (lambda id body)
    (abstraction (%lambda (id) body)))

  (define-rules-syntax
    ((a-lambda (id t) body)
      (abstraction-type t (%lambda (id) body))))

  (define-rule-syntax (apply lhs rhs)
    (application lhs rhs))

  (define-rule-syntax (recursive id body)
    (%recursive (%lambda (id) body)))

  (define-rule-syntax (if a b c)
    (branch a b c))

  (define-rule-syntax (check=? in out)
    (check
      (equal?
        (term->datum 0 #f in)
        (term->datum 0 #f out))))
)
