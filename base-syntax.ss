(library (base-syntax)
  (export
    boolean->datum
    number->datum
    string->datum)

  (import
    (scheme)
    (base)
    (syntax))

  (define-syntax-rule (boolean->datum $boolean) $boolean)
  (define-syntax-rule (number->datum $number) $number)
  (define-syntax-rule (string->datum $string) $string)
)
