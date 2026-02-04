(library (micalang runtime)
  (export
    literal app
    type bool int
    inc dec = + - < zero?
    list)
  (import
    (except (micalang base) = + - < zero? list)
    (rename (micalang term) (pi %pi)))
  (export
    (import
      (only (micascheme) lambda equal? app)
      (only (micalang term) native)))

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
