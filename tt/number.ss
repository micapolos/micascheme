(library (tt number)
  (export = number=? + - zero?)
  (import
    (tt lang)
    (prefix (chezscheme) %))

  (define = (unchecked (pi (number number) boolean) %=))
  (define number=? (unchecked (pi (number number) boolean) %=))
  (define + (unchecked (pi (number number) number) %+))
  (define - (unchecked (pi (number number) number) %-))
  (define zero? (unchecked (pi (number) boolean) %zero?))
)
