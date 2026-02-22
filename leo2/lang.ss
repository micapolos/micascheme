(library (leo2 lang)
  (export
    any
    nothing anything
    quote
    type
    native native-apply
    indexed
    symbolic
    the
    lambda
    recursive
    apply
    if
    annotated
    evaluated
    typed
    check-lang)
  (import
    (rename (leo2 base)
      (lambda %lambda)
      (quote %quote)
      (apply %apply)
      (if %if))
    (prefix (leo2 term) %)
    (leo2 datum))

  (define-syntax nothing (identifier-syntax %nothing))
  (define-syntax anything (identifier-syntax %anything))

  (define-keywords any)

  (define-rules-syntaxes (literals any)
    ((type n)
      (nonnegative-integer? (datum n))
      (%type n))
    ((quote s)
      (symbol? (datum s))
      (%quote s))
    ((indexed i x)
      (nonnegative-integer? (datum i))
      (%indexed i x))
    ((symbolic s x)
      (symbol? (datum s))
      (%symbolic 's x))
    ((native x)
      (%native x))
    ((native-apply fn arg ...)
      (%native-application fn (list arg ...)))
    ((the x)
      (symbol? (datum x))
      (%variable 'x))
    ((lambda id body)
      (identifier? #'id)
      (%abstraction (%lambda (id) body)))
    ((lambda (any id t) body)
      (identifier? #'id)
      (%signature t (%lambda (id) body)))
    ((apply lhs rhs)
      (%application lhs rhs))
    ((recursive id body)
      (identifier? #'id)
      (%recursion (%lambda (id) body)))
    ((if a b c)
      (%branch a b c))
    ((annotated ann x)
      (%annotated ann x))
    ((evaluated x)
      (%evaluated x))
    ((typed t v)
      (%typed t v)))

  (define-rule-syntax (check-lang in ...)
    (begin
      (check-term->datum=? in in) ...))
)
