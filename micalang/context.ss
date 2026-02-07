(library (micalang context)
  (export mica-context)
  (import
    (only (micalang base) cons define quote quasiquote unquote define-rules-syntax define-rule-syntax quote quasiquote unquote ...)
    (micalang typed)
    (micalang comptime))

  (define-rules-syntax
    ((entry id type)
      (cons 'id type)))

  (define-rule-syntax (context x ...)
    `(,(entry . x) ...))

  (define mica-context
    (context
      (type   type)
      (boolean   type)
      (number    type)
      (string type)

      (zero?  (pi number boolean))
      (inc    (pi number number))
      (dec    (pi number number))

      (=      (pi number (pi number number)))
      (+      (pi number (pi number number)))
      (-      (pi number (pi number number)))
      (<      (pi number (pi number boolean)))

      (list   (pi (x type) x))

      (index        (pi (n number) type))
      (first-index  (pi (n number) (index n)))
      (last-index   (pi (n number) (index n)))

      (array        (pi (n number) type))))
)

