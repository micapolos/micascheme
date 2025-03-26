(library (typed-scheme expr-syntax)
  (export syntax->expr)
  (import
    (micascheme)
    (typed-scheme keywords)
    (typed-scheme type)
    (typed-scheme types)
    (typed-scheme expr)
    (typed-scheme type-syntax))

  (define (scope+ $scope $identifier $type)
    (push $scope (cons $identifier $type)))

  (define (scope-ref $scope $identifier)
    (scope-ref-from $scope $identifier 0))

  (define (scope-ref-from $scope $identifier $index)
    (switch $scope
      ((null? _)
        #f)
      ((pair? (pair $binding $scope))
        (if (free-identifier=? $identifier (car $binding))
          (expr (cdr $binding) (variable-term $index))
          (scope-ref-from $scope $identifier (+ $index 1))))))

  (define (syntax->expr $lookup $type-scope $scope $syntax)
    (syntax-case $syntax ()
      (n
        (fixnum? (datum n))
        (expr fixnum-type (native-term (datum n))))
      (n
        (integer? (datum n))
        (expr integer-type (native-term (datum n))))
      (n
        (number? (datum n))
        (expr number-type (native-term (datum n))))
      (other
        (core-syntax->expr syntax->type syntax->expr $lookup $type-scope $scope #'other))))

  (define (core-syntax->expr $type-recurse $recurse $lookup $type-scope $scope $syntax)
    (syntax-case $syntax (lambda)
      (x
        (and (identifier? #'x) (scope-ref $scope #'x))
        (scope-ref $scope #'x))
      ((lambda ((type name) ...) body)
        (for-all identifier? (syntaxes name ...))
        (lets
          ($param-names (syntaxes name ...))
          ($param-types
            (map
              (partial $type-recurse $lookup $type-scope)
              (syntaxes type ...)))
          ($scope
            (fold-left scope+ $scope $param-names $param-types))
          ($body-expr
            ($recurse $lookup $type-scope $scope #'body))
          (expr
            (lambda-type 0
              (list->immutable-vector $param-types)
              (expr-type $body-expr))
            (lambda-term (expr-term $body-expr)))))))
)
