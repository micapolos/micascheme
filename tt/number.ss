(library (tt number)
  (export = + - * zero?)
  (import
    (tt lang)
    (prefix (chezscheme) %))

  (define = (unchecked (pi (number number) boolean) %=))
  (define + (unchecked (pi (number number) number) %+))
  (define - (unchecked (pi (number number) number) %-))
  (define * (unchecked (pi (number number) number) %*))
  (define zero? (unchecked (pi (number) boolean) %zero?))
)
