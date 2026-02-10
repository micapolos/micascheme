(library (micalang environment)
  (export mica-environment)
  (import
    (only (micalang base) define define-rule-syntax quasiquote unquote ...)
    (micalang comptime))

  (define-rule-syntax (environment (id value type) ...)
    `((id ,value ,type) ...))

  (define mica-environment
    (environment
    ; (symbol  value   type)
      (type    type    type)
      (boolean boolean type)
      (number  number  type)
      (char    char    type)
      (string  string  type)

      (zero?   (curry %%zero? a) (pi number boolean))

      (=       (curry %%= a b) (pi number (pi number number)))
      (+       (curry %%+ a b) (pi number (pi number number)))
      (-       (curry %%- a b) (pi number (pi number number)))
      (<       (curry %%< a b) (pi number (pi number boolean)))))
)
