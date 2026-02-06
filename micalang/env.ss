(library (micalang env)
  (export mica-env)
  (import
    (only (micalang base) define quote quasiquote unquote define-rules-syntax define-rule-syntax quote quasiquote unquote ...)
    (micalang typed)
    (micalang comptime))

  (define-rules-syntax
    ((entry id type datum)
      `(id ,(typed type 'datum)))
    ((entry id type)
      (entry id type id)))

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

      (list   (pi (x type) x))))
)
