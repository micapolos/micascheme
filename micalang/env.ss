(library (micalang env)
  (export mica-env)
  (import
    (micalang base)
    (micalang compiled)
    (prefix (micalang comptime) %))

  (define mica-env
    `(
      (type    . ,%type)
      (boolean . ,%boolean)
      (number  . ,%number)
      (char    . ,%char)
      (string  . ,%string)
      (symbol  . ,%symbol)

      (zero?   . ,(%curry zero? a))

      (=       . ,(%curry = a b))
      (+       . ,(%curry + a b))
      (-       . ,(%curry - a b))
      (<       . ,(%curry < a b))))
)

