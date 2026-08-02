(library (tt number)
  (export = +)
  (import
    (tt lang)
    (prefix (scheme) %))

  (define = (unchecked (pi (number number) boolean) %=))
  (define + (unchecked (pi (number number) number) %+))
)
