(library (leo2 lang)
  (export
    nothing anything
    quote
    type
    native native-apply
    variable
    lambda
    recursive
    apply
    if
    labeled
    evaluated
    typed
    check-lang)
  (import
    (rename (leo2 base)
      (lambda %lambda)
      (apply %apply)
      (quote %quote)
      (if %if))
    (prefix (leo2 term) %)
    (leo2 datum))

  (define-syntax nothing (identifier-syntax %nothing))
  (define-syntax anything (identifier-syntax %anything))

  (define-rules-syntaxes
    ((type n)
      (nonnegative-integer? (datum n))
      (%type n))
    ((quote s)
      (%quoted (%quote s)))
    ((native x)
      (%native x))
    ((native-apply fn arg ...)
      (%native-application fn (list arg ...)))
    ((variable n)
      (nonnegative-integer? (datum n))
      (%variable n))
    ((lambda id body)
      (identifier? #'id)
      (%lambda (id) body))
    ((lambda (id t) body)
      (identifier? #'id)
      (%signature t (%lambda (id) body)))
    ((apply lhs rhs)
      (%application lhs rhs))
    ((recursive id body)
      (identifier? #'id)
      (%recursion (%lambda (id) body)))
    ((if a b c)
      (%branch a b c))
    ((labeled ann x)
      (%labeled ann x))
    ((evaluated x)
      (%evaluated x))
    ((typed t v)
      (%typed t v)))

  (define-rule-syntax (check-lang in ...)
    (begin
      (check-term->datum=? in in) ...))
)
