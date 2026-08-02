(library (tt number)
  (export = + number=?)
  (import
    (tt lang)
    (prefix (scheme) %))

  (define = (unchecked (pi (number number) boolean) %=))
  (define number=? =)
  (define + (unchecked (pi (number number) number) %+))
)
