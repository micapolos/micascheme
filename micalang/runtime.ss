(library (micalang runtime)
  (export
    literal app
    type bool int
    inc dec = + - < zero?
    list lambda)
  (import
    (except (micalang base) = + - < zero? list lambda)
    (prefix (only (micalang base) lambda) %)
    (rename (micalang term) (pi %pi)))
  (export
    (import
      (only (micascheme) equal? app)
      (only (micalang term) native)))

  (define-rules-syntax
    ((lambda (id) body)
      (%lambda (id) body))
    ((lambda (id ids ...) body)
      (lambda id
        (lambda ids ... body))))

  (define-rule-syntax (literal x) x)

  (define type (native 'type))
  (define bool (native 'bool))
  (define int (native 'int))

  (define zero? (lambda (x) (fxzero? x)))
  (define inc (lambda (x) (fx+/wraparound x 1)))
  (define dec (lambda (x) (fx-/wraparound x 1)))

  (define = (lambda (x) (lambda (y) (fx= x y))))
  (define + (lambda (x) (lambda (y) (fx+/wraparound x y))))
  (define - (lambda (x) (lambda (y) (fx-/wraparound x y))))
  (define < (lambda (x) (lambda (y) (fx< x y))))

  (define list (lambda (x) (application list x)))
)
