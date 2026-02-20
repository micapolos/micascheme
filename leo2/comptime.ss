(library (leo2 comptime)
  (export
    type
    native native-apply
    symbolic unit
    variable
    lambda a-lambda
    apply recursion
    if
    check=?)
  (import
    (leo2 datum)
    (leo2 stdlib)
    (rename (leo2 base)
      (lambda %lambda)
      (apply %apply)
      (if %if))
    (rename (leo2 term)
      (type %type)
      (symbolic %symbolic)
      (unit %unit)
      (typed %typed)
      (native %native)
      (variable %variable)
      (recursion %recursion)))
  (export
    (import
      (only (leo2 base) quote)))

  (define-rule-syntax (type n)
    (%type n))

  (define-rule-syntax (typed t v)
    (%typed t v))

  (define-rule-syntax (native t x)
    (native-term t x))

  (define-rule-syntax (native-apply t fn arg ...)
    (native-application-term t fn arg ...))

  (define-rule-syntax (symbolic symbol x)
    (symbolic-term 'symbol x))

  (define-syntax (unit $syntax)
    (syntax-case $syntax ()
      (id
        (identifier? #'id)
        #'unit-term)))

  (define-rule-syntax (variable t x)
    (variable-term t 'x))

  (define-rule-syntax (lambda (id t) body)
    (abstraction-term t (%lambda (id) body)))

  (define-rule-syntax (a-lambda (id t) body)
    (abstraction-type-term t (%lambda (id) body)))

  (define-rule-syntax (apply lhs rhs)
    (application-term lhs rhs))

  (define-rule-syntax (recursion id t body)
    (typed t (%recursion (%lambda (id) body))))

  (define-rule-syntax (if a b c)
    (branch a b c))

  (define-rule-syntax (check=? in out)
    (check
      (equal?
        (term->datum #f #f 0 in)
        (term->datum #f #f 0 out))))
)
