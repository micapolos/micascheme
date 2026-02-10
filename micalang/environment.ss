(library (micalang environment)
  (export mica-environment)
  (import
    (only (micalang base) define define-rule-syntax quasiquote unquote ...)
    (micalang comptime))

  (define-rule-syntax (environment (id value type) ...)
    `((id ,value ,type) ...))

  (define mica-environment
    (environment
    ; (symbol  type   value)
      (type    type   type)
      (boolean type   boolean)
      (number  type   number)
      (char    type   char)
      (string  type   string)

      (zero?   (pi number boolean)              (curry %%zero? a))

      (=       (pi number (pi number number))   (curry %%= a b))
      (+       (pi number (pi number number))   (curry %%+ a b))
      (-       (pi number (pi number number))   (curry %%- a b) )
      (<       (pi number (pi number boolean))  (curry %%< a b) )))
)
