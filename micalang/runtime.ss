(library (micalang runtime)
  (export
    literal app
    type bool int
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
    ((app lhs rhs)
      (%app lhs rhs))
    ((app lhs rhs rhss ...)
      (app (app lhs rhs) rhss ...)))

  (define-rule-syntax (literal x) x)

  (define type (native 'type))
  (define bool (native 'bool))
  (define int (native 'int))

  (define zero? (lambda x (fxzero? x)))
  (define inc (lambda x (fx+/wraparound x 1)))
  (define dec (lambda x (fx-/wraparound x 1)))

  (define = (lambda x y (fx= x y)))
  (define + (lambda x y (fx+/wraparound x y)))
  (define - (lambda x y (fx-/wraparound x y)))
  (define < (lambda x y (fx< x y)))

  (define list (lambda x (application list x)))
)
