(import
  (micascheme)
  (syntax lookup)
  (typed-scheme type)
  (typed-scheme types)
  (typed-scheme expr)
  (typed-scheme expr-syntax))

(define $lookup
  (lookup-with
    (a-boolean boolean-type-definition)
    (a-string string-type-definition)
    (a-number number-type-definition)
    (a-pair pair-type-definition)))

(check
  (equal?
    (syntax->expr $lookup (stack) (stack) #'123.4)
    (expr number-type (native-term 123.4))))

(check
  (equal?
    (syntax->expr $lookup (stack) (stack) #'(lambda ((a-string s)) s))
    (expr
      (lambda-type 0 (immutable-vector string-type) string-type)
      (lambda-term (variable-term 0)))))
