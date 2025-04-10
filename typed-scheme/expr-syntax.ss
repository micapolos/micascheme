(library (typed-scheme expr-syntax)
  (export syntax->expr expr->syntax)
  (import
    (micascheme)
    (typed-scheme keywords)
    (typed-scheme type)
    (typed-scheme types)
    (typed-scheme expr)
    (typed-scheme type-datum)
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

  (define (syntax->expr-of $recurse $type-definition-lookup $type-lookup $type-scope $scope $type $syntax)
    (lets
      ($expr ($recurse $type-definition-lookup $type-lookup $type-scope $scope $syntax))
      (if (type-assignable-to? (expr-type $expr) $type)
        $expr
        (syntax-error $syntax
          (format "invalid type ~s, expected ~s in"
            (type->datum (expr-type $expr))
            (type->datum $type))))))

  (define (syntax->lambda-expr $recurse $type-definition-lookup $type-lookup $type-scope $scope $syntax)
    (lets
      ($expr ($recurse $type-definition-lookup $type-lookup $type-scope $scope $syntax))
      (switch (expr-type $expr)
        ((lambda-type? $lambda-type) $expr)
        ((else $type)
          (syntax-error $syntax
            (format "invalid type ~s, expected any-lambda in" $type))))))

  (define (syntax->expr $type-recurse $recurse $type-definition-lookup $type-lookup $type-scope $scope $syntax)
    (syntax-case $syntax (assume lambda let expect if)
      (x
        (and (identifier? #'x) (scope-ref $scope #'x))
        (scope-ref $scope #'x))
      (x
        (and (identifier? #'x) ($type-lookup #'x))
        (expr ($type-lookup #'x) (native-term #'x)))
      ((assume type body)
        (expr
          ($type-recurse $type-definition-lookup $type-scope #'type)
          (native-term #'body)))
      ((lambda ((type name) ...) body)
        (for-all identifier? (syntaxes name ...))
        (lets
          ($param-names (syntaxes name ...))
          ($param-types
            (map
              (partial $type-recurse $type-definition-lookup $type-scope)
              (syntaxes type ...)))
          ($scope
            (fold-left scope+ $scope $param-names $param-types))
          ($body-expr
            ($recurse $type-definition-lookup $type-lookup $type-scope $scope #'body))
          (expr
            (lambda-type $param-types (expr-type $body-expr))
            (lambda-term $param-types $body-expr))))
      ((let ((name exp) ...) body)
        (for-all identifier? (syntaxes name ...))
        (lets
          ($names (syntaxes name ...))
          ($exprs (map ($recurse $type-definition-lookup $type-lookup $type-scope $scope) (syntaxes exp ...)))
          ($types (map expr-type $exprs))
          ($scope (fold-left scope+ $scope $names $types))
          ($body-expr ($recurse $type-definition-lookup $type-lookup $type-scope $scope #'body))
          (expr
            (expr-type $body-expr)
            (bind-term $exprs $body-expr))))
      ((if condition consequent alternate)
        (lets
          ($condition-expr
            (syntax->expr-of
              $recurse
              $type-definition-lookup
              $type-lookup
              $type-scope
              $scope
              boolean-type
              #'condition))
          ($consequent-expr
            ($recurse
              $type-definition-lookup
              $type-lookup
              $type-scope
              $scope
              #'consequent))
          ($alternate-expr
            ($recurse
              $type-definition-lookup
              $type-lookup
              $type-scope
              $scope
              #'alternate))
          (expr
            (type+ (expr-type $consequent-expr) (expr-type $alternate-expr))
            (if-term $condition-expr $consequent-expr $alternate-expr))))
      ((expect type expr)
        (syntax->expr-of
          $recurse
          $type-definition-lookup
          $type-lookup
          $type-scope
          $scope
          ($type-recurse $type-definition-lookup $type-scope #'type)
          #'expr))
      ((fn arg ...)
        (lets
          ($lambda-expr (syntax->lambda-expr $recurse $type-definition-lookup $type-lookup $type-scope $scope #'fn))
          ($lambda-type (expr-type $lambda-expr))
          ($arg-exprs
            (map*
              (lambda ($type $arg)
                (syntax->expr-of $recurse $type-definition-lookup $type-lookup $type-scope $scope $type $arg))
              (lambda ($type $varargs)
                (map
                  (partial syntax->expr-of $recurse $type-definition-lookup $type-lookup $type-scope $scope $type)
                  $varargs))
              (lambda-type-params $lambda-type)
              (syntaxes arg ...)))
          (expr
            (lambda-type-result $lambda-type)
            (application-term $lambda-expr $arg-exprs))))))

  (define (scope-gensym $scope $id $index)
    (datum->syntax $id
      (gensym
        (string-append "v"
          (number->string (+ (length $scope) $index))))))

  (define (expr->syntax $id $native $scope $expr)
    (switch (expr-term $expr)
      ((native-term? $native-term)
        ($native (native-term-value $native-term)))
      ((variable-term? (variable-term $index))
        (list-ref $scope $index))
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
      ((application-term? (application-term $lambda-expr $arg-exprs))
        #`(
          #,(expr->syntax $id $native $scope $lambda-expr)
          #,@(map (partial expr->syntax $id $native $scope) $arg-exprs)))
      ((if-term? (if-term $condition $consequent $alternate))
        #`(if
          #,(expr->syntax $id $native $scope $condition)
          #,(expr->syntax $id $native $scope $consequent)
          #,(expr->syntax $id $native $scope $alternate)))))
)
