(library (micalang runtime)
  (export
    literal app
    inc dec = + - < zero?
    list let lambda app
    first-index last-index prim)
  (import
    (except (micalang base) = + - < zero? list lambda app let)
    (prefix (only (micalang base) let lambda app) %)
    (rename (micalang term) (pi %pi)))
  (export
    (import
      (only (micascheme) equal? from $primitive)
      (only (micalang term) native)))

  (define-rule-syntax (let (id x) ... body)
    (%let ((id x) ...) body))

  (define-rules-syntax
    ((lambda id body)
      (%lambda (id) body)))

  (define-rules-syntax
    ((prim x) x)
    ((prim a x) (lambda a (x a)))
    ((prim a b x) (lambda a (lambda b (x a b))))
    ((prim a b c x) (lambda a (lambda b (lambda c (x a b c)))))
    ((prim a b c d x) (lambda a (lambda b (lambda c (lambda d (x a b c d)))))))

  (define-rules-syntax
    ((define-prim id arg ... p)
      (define id (prim arg ... p))))

  (define-rule-syntax (define-prims (id arg ... prim) ...)
    (begin (define-prim id arg ... prim) ...))

  (define-rules-syntax
    ((app lhs rhs)
      (%app lhs rhs)))

  (define-rule-syntax (literal x) x)

  (define (%first-index _) 0)
  (define (%last-index n) (fx-1/wraparound n))

  (define-prims
    (zero? x fxzero?)
    (inc x fx+1/wraparound)
    (dec x fx-1/wraparound)

    (= x y fx=)
    (+ x y fx+/wraparound)
    (- x y fx-/wraparound)
    (< x y fx<)
    (first-index n %first-index)
    (last-index   n %last-index))

  (define list (lambda x (application list x)))
)
