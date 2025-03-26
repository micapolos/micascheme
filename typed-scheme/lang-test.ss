(import
  (micascheme)
  (typed-scheme lang)
  (typed-scheme type)
  (typed-scheme types))

(check (equal? (type a-boolean) boolean-type))
(check (equal? (type a-string) string-type))
(check (equal? (type a-number) number-type))

(check
  (equal?
    (type (a-lambda (a-boolean a-string) a-number))
    (lambda-type
      (immutable-vector boolean-type string-type)
      number-type)))

(check
  (equal?
    (type (oneof a-boolean a-string))
    (union-type
      (immutable-vector boolean-type string-type))))

(check
  (equal?
    (type (forall (i o) (a-lambda (i) o)))
    (forall-type 2
      (lambda-type
        (immutable-vector (variable-type 1))
        (variable-type 0)))))
