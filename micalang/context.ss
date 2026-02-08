(library (micalang context)
  (export mica-context)
  (import
    (only (micalang base) cons define quote quasiquote unquote define-rules-syntax define-rule-syntax quote quasiquote unquote ...)
    (micalang compiled)
    (micalang comptime))

  (define-rules-syntax
    ((entry id type)
      (cons 'id type)))

  (define-rule-syntax (context x ...)
    `(,(entry . x) ...))

  (define mica-context
    (context
      (type    type)
      (boolean type)
      (number  type)
      (char    type)
      (string  type)
      (symbol  type)

      (zero?  (pi number boolean))
      (inc    (pi number number))
      (dec    (pi number number))

      (=      (pi number (pi number number)))
      (+      (pi number (pi number number)))
      (-      (pi number (pi number number)))
      (<      (pi number (pi number boolean)))))
)

