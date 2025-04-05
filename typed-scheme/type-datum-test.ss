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

(check
  (equal?
    (scope-type->datum (stack 'v0 'v1) (variable-type 0))
    'v1))

(check
  (equal?
    (scope-type->datum (stack 'v0 'v1) (variable-type 1))
    'v0))

; (check
;   (equal?
;     (type->datum (forall-type (list in-variance out-variance) (native-type 'a)))
;     'v0))
