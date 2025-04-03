(import
  (micascheme)
  (typed-scheme type-datum)
  (typed-scheme type)
  (typed-scheme types))

(check
  (equal?
    (type->datum (lambda-type (list string-type number-type) boolean-type))
    '(any-lambda (any-string any-number) any-boolean)))

(check
  (equal?
    (type->datum (lambda-type (list* string-type number-type boolean-type) boolean-type))
    '(any-lambda (any-string any-number . any-boolean) any-boolean)))

(check
  (equal?
    (type->datum (lambda-type (list* boolean-type) boolean-type))
    '(any-lambda any-boolean any-boolean)))
