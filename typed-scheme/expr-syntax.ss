(library (typed-scheme expr-syntax)
  (export
    typed typed? typed-type typed-value
    syntax->expr
    syntax->expr-datum)
  (import
    (micascheme)
    (typed-scheme keywords)
    (typed-scheme type)
    (typed-scheme types)
    (typed-scheme type-syntax))

  (data (typed type value))

  (define (scope+ $scope $identifier $type)
    (push $scope (cons $identifier $type)))

  (define (scope-ref $scope $identifier)
    (switch $scope
      ((null? _)
        #f)
      ((pair? (pair $binding $scope))
        (if (free-identifier=? $identifier (car $binding))
          (cdr $binding)
          (scope-ref $scope $identifier)))))

  (define (syntax->expr-datum $lookup $type-scope $scope $syntax)
    (lets
      ($typed (syntax->expr $lookup $type-scope $scope $syntax))
      (typed-with-value $typed (syntax->datum (typed-value $typed)))))

  (define (syntax->expr $lookup $type-scope $scope $syntax)
    (syntax-case $syntax ()
      (n
        (fixnum? (datum n))
        (typed fixnum-type #'n))
      (n
        (integer? (datum n))
        (typed integer-type #'n))
      (n
        (number? (datum n))
        (typed number-type #'n))
      (other
        (core-syntax->expr syntax->type syntax->expr $lookup $type-scope $scope #'other))))

  (define (core-syntax->expr $type-recurse $recurse $lookup $type-scope $scope $syntax)
    (syntax-case $syntax (lambda)
      (x
        (and (identifier? #'x) (scope-ref $scope #'x))
        (typed (scope-ref $scope #'x) #'x))
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
          ($typed-body
            ($recurse $lookup $type-scope $scope #'body))
          (typed
            (lambda-type
              (list->immutable-vector $param-types)
              (typed-type $typed-body))
            #`(lambda (name ...)
              #,(typed-value $typed-body)))))))
)
