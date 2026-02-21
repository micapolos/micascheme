(library (leo2 comptime)
  (export
    type
    native native-apply
    indexed
    symbolic
    symbol a-symbol
    literal
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
      (indexed %indexed)
      (symbolic %symbolic)
      (typed %typed)
      (native %native)
      (variable %variable)
      (recursion %recursion)))
  (export
    (import
      (only (leo2 base) quote)))

  (define-rules-syntaxes
    ((type n) (%type n))
    ((typed t v) (%typed t v))
    ((native t x) (native-term t x))
    ((literal x)
      (boolean? (datum x))
      (boolean-term x))
    ((literal x)
      (number? (datum x))
      (number-term x))
    ((literal x)
      (char? (datum x))
      (char-term x))
    ((literal x)
      (string? (datum x))
      (string-term x))
    ((native-apply t fn arg ...)
      (native-application-term t fn arg ...))
    ((indexed index x)
      (indexed-term index x))
    ((symbol x)
      (symbol-term 'x))
    ((a-symbol x)
      (symbol-type-term 'x))
    ((symbolic symbol x)
      (symbolic-term 'symbol x))
    ((variable t x)
      (variable-term t 'x))
    ((lambda (id t) body)
      (abstraction-term t (%lambda (id) body)))
    ((a-lambda (id t) body)
      (abstraction-type-term t (%lambda (id) body)))
    ((apply lhs rhs)
      (application-term lhs rhs))
    ((recursion id t body)
      (typed t (%recursion (%lambda (id) body))))
    ((if a b c)
      (branch a b c))
    ((check=? in out)
      (check
        (equal?
          (term->datum #f #f 0 in)
          (term->datum #f #f 0 out)))))
)
