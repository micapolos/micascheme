(import
  (micascheme)
  (syntax lookup)
  (typed-scheme type)
  (typed-scheme types)
  (typed-scheme expr-syntax))

(define $lookup
  (lookup-with
    (a-boolean boolean-type-definition)
    (a-string string-type-definition)
    (a-number number-type-definition)
    (a-pair pair-type-definition)))

(check
  (equal?
    (syntax->expr-datum $lookup (stack) (stack) #'123.4)
    (typed number-type 123.4)))

(check
  (equal?
    (syntax->expr-datum $lookup (stack) (stack) #'(lambda ((a-string s)) s))
    (typed
      (lambda-type 0 (immutable-vector string-type) string-type)
      '(lambda (s) s))))
