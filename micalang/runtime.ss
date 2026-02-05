(library (micalang runtime)
  (export
    literal app
    inc dec = + - < zero?
    list let lambda app)
  (import
    (except (micalang base) = + - < zero? list lambda app let)
    (prefix (only (micalang base) let lambda app) %)
    (rename (micalang term) (pi %pi)))
  (export
    (import
      (only (micascheme) equal?)
      (only (micalang term) native)))

  (define-rule-syntax (let (id x) ... body)
    (%let ((id x) ...) body))

  (define-rules-syntax
    ((lambda id body)
      (%lambda (id) body))
    ((lambda id ids ... body)
      (lambda id
        (lambda ids ... body))))

  (define-rules-syntax
    ((define-prim id prim)
      (define id prim))
    ((define-prim id x prim)
      (define id
        (lambda x (prim x))))
    ((define-prim id x y prim)
      (define id
        (lambda x y (prim x y)))))

  (define-rule-syntax (define-prims (id arg ... prim) ...)
    (begin (define-prim id arg ... prim) ...))

  (define-rules-syntax
    ((app lhs rhs)
      (%app lhs rhs))
    ((app lhs rhs rhss ...)
      (app (app lhs rhs) rhss ...)))

  (define-rule-syntax (literal x) x)

  (define-prims
    (zero? x fxzero?)
    (inc x fx+1/wraparound)
    (dec x fx-1/wraparound)

    (= x y fx=)
    (+ x y fx+/wraparound)
    (- x y fx-/wraparound)
    (< x y fx<))

  (define list (lambda x (application list x)))
)
