(library (tt number)
  (export = + number=? number->string)
  (import
    (tt lang)
    (prefix (chezscheme) %))

  (define = (unchecked (pi (number number) boolean) %=))
  (define number=? =)
  (define + (unchecked (pi (number number) number) %+))
  (define number->string (unchecked (pi (number) string) %number->string))
)
