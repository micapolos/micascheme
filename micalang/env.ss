(library (micalang env)
  (export mica-env)
  (import
    (only (micalang base) cons define quote quasiquote unquote define-rules-syntax define-rule-syntax quote quasiquote unquote ...)
    (micalang typed)
    (micalang comptime))

  (define-rules-syntax
    ((entry id type)
      (cons 'id type)))

  (define-rule-syntax (env x ...)
    `(,(entry . x) ...))

  (define mica-env
    (env
      (type   type)
      (bool   type)
      (int    type)
      (string type)

      (zero?  (pi int bool))
      (inc    (pi int int))
      (dec    (pi int int))

      (=      (pi int (pi int int)))
      (+      (pi int (pi int int)))
      (-      (pi int (pi int int)))
      (<      (pi int (pi int bool)))

      (list   (pi (x type) x))

      (index        (pi (n int) type))
      (first-index  (pi (n int) (index n)))
      (last-index   (pi (n int) (index n)))

      (array        (pi (n int) type))))
)

