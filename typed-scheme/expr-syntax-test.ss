(import
  (micascheme)
  (syntax lookup)
  (typed-scheme type)
  (typed-scheme types)
  (typed-scheme expr)
  (typed-scheme type-syntax)
  (typed-scheme expr-syntax))

(define $lookup
  (lookup-with
    (a-boolean boolean-type-definition)
    (a-string string-type-definition)
    (a-number number-type-definition)
    (a-pair pair-type-definition)))

(define (test-syntax->type $lookup $scope $syntax)
  (syntax->type test-syntax->type $lookup $scope $syntax))

(define (test-syntax->expr $lookup $type-scope $scope $syntax)
  (syntax-case $syntax ()
    (n
      (string? (datum n))
      (expr string-type (native-term (datum n))))
    (other
      (syntax->expr test-syntax->type test-syntax->expr $lookup $type-scope $scope #'other))))

(check
  (equal?
    (test-syntax->expr $lookup (stack) (stack) #'"foo")
    (expr string-type (native-term "foo"))))

(check
  (equal?
    (test-syntax->expr $lookup (stack) (stack) #'(lambda ((a-string s)) s))
    (expr
      (lambda-type 0 (immutable-vector string-type) string-type)
      (lambda-term (variable-term 0)))))
