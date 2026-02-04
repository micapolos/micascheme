(library (micalang env)
  (export mica-env)
  (import
    (only (micalang base) define quote quasiquote unquote)
    (micalang typed)
    (micalang comptime))

  (define mica-env
    `(
      (type  ,(typed type 'type))
      (bool  ,(typed type 'bool))
      (int   ,(typed type 'int))

      (zero? ,(typed (pi int bool) 'zero?))
      (inc   ,(typed (pi int int)  'inc))
      (dec   ,(typed (pi int int)  'dec))

      (=     ,(typed (pi int (pi int int))  '-))
      (+     ,(typed (pi int (pi int int))  '+))
      (-     ,(typed (pi int (pi int int))  '-))
      (<     ,(typed (pi int (pi int bool)) '<))

      (list  ,(typed (pi (x type) x) 'list))))
)
