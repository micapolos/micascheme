(library (typed-scheme expr-syntax)
  (export syntax->expr expr->syntax)
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

  (define (syntax->expr $type-recurse $recurse $lookup $type-scope $scope $syntax)
    (syntax-case $syntax (lambda let)
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
            (lambda-term
              (list->immutable-vector $param-types)
              $body-expr))))
      ((let ((name exp) ...) body)
        (for-all identifier? (syntaxes name ...))
        (lets
          ($names (syntaxes name ...))
          ($exprs (map ($recurse $lookup $type-scope $scope) (syntaxes exp ...)))
          ($types (map expr-type $exprs))
          ($scope (fold-left scope+ $scope $names $types))
          ($body-expr ($recurse $lookup $type-scope $scope #'body))
          (expr
            (expr-type $body-expr)
            (bind-term $exprs $body-expr))))))

  (define (scope-gensym $scope $id $index)
    (datum->syntax $id
      (gensym
        (string-append "v"
          (number->string (+ (length $scope) $index))))))

  (define (expr->syntax $id $native $scope $expr)
    (switch (expr-term $expr)
      ((native-term? $native-term)
        ($native (native-term-value $native-term)))
      ((bind-term? (bind-term $bound-exprs $body-expr))
        (lets
          ($tmps (map (partial scope-gensym $scope $id) (iota (length $bound-exprs))))
          ($bound-syntaxes (map (partial expr->syntax $id $native $scope) $bound-exprs))
          ($entries
            (map-with
              ($tmp $tmps)
              ($bound-syntax $bound-syntaxes)
              #`(#,$tmp #,$bound-syntax)))
          ($scope (fold-left push $scope $tmps))
          #`(let (#,@$entries)
            #,(expr->syntax $id $native $scope $body-expr))))
      ((lambda-term? (lambda-term $param-types $body-expr))
        (lets
          ($tmps (map (partial scope-gensym $scope $id) (iota (length $param-types))))
          ($scope (fold-left push $scope $tmps))
          #`(lambda (#,@$tmps)
            #,(expr->syntax $id $native $scope $body-expr))))
      ((variable-term? (variable-term $index))
        (list-ref $scope $index))))
)
