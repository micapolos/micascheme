(library (typed evaluate)
  (export
    evaluate-typed
    evaluate-value
    evaluate-type
    evaluate-lambda
    evaluate-identifier
    evaluate-param
    evaluate-binding

    any-environment any-environment?
    any-scope any-scope
    any-typed any-typed?
    any-evaluate-typed-lambda)
  (import
    (micascheme)
    (evaluator)
    (any)
    (typed lang)
    (typed typed)
    (typed type)
    (typed compiled)
    (typed thunk)
    (typed evaluated)
    (typed scope)
    (typed hole)
    (typed combo))

  (data any-environment)
  (data any-scope)
  (data any-typed)

  (define any-evaluate-typed-lambda
    (any-lambda
      (
        (any-lambda (any-environment any-scope any-syntax) any-typed)
        any-environment
        any-scope
        any-syntax
        (any-lambda () any-typed))
      any-typed))

  (define (evaluate-identifier $scope $identifier)
    (lets
      ($symbol (syntax->datum $identifier))
      ((pair $index $typed)
        (or
          (scope-ref $scope $symbol)
          (syntax-error $identifier "undefined")))
      ($type (typed-type $typed))
      (typed $type
        (switch (typed-value $typed)
          ((hole? _)
            (thunk $index (compiled (stack) $symbol)))
          ((combo? $combo)
            $combo)))))

  (define (evaluate-value $recurse $environment $scope $type $syntax)
    (lets
      ($typed ($recurse $environment $scope $syntax))
      (if (type=? (typed-type $typed) $type)
        (typed-value $typed)
        (syntax-error $syntax
          (format "invalid type ~s, expected ~s, in"
            (typed-type $typed)
            $type)))))

  (define (evaluate-lambda $recurse $environment $scope $syntax)
    (lets
      ($typed ($recurse $environment $scope $syntax))
      (switch (typed-type $typed)
        ((any-lambda? _)
          $typed)
        ((else $type)
          (syntax-error $syntax
            (format "invalid type ~s, expected any-lambda, in" $type))))))

  (define (evaluate-type $recurse $environment $scope $syntax)
    (switch (evaluate-value $recurse $environment $scope any-type $syntax)
      ((thunk? _)
        (syntax-error $syntax "type not constant"))
      ((combo? $combo)
        (combo-value $combo))))

  (define (evaluate-typed $recurse $environment $scope $syntax)
    (evaluate-typed-discard $recurse $environment $scope $syntax
      (lambda ()
        (evaluate-typed-expanded $recurse $environment $scope $syntax))))

  (define (evaluate-typed-discard $recurse $environment $scope $syntax $discard)
    (switch $scope
      ((null? _)
        ($discard))
      ((pair? $pair)
        (lets
          ((pair $binding $scope) $pair)
          ((pair $symbol $typed) $binding)
          ((typed $type $bound) $typed)
          (cond
            (
              (and
                (type=? $type any-evaluate-typed-lambda)
                (not (hole? $bound)))
              ($bound $recurse $environment $scope $syntax
                (lambda ()
                  (evaluate-typed-discard $recurse $environment $scope $syntax $discard))))
            (else
              (evaluate-typed-discard $recurse $environment $scope $syntax $discard)))))))

  (define (evaluate-typed-expanded $recurse $environment $scope $syntax)
    (syntax-case $syntax (assume assume-type any-lambda lambda expect let)
      (x
        (symbol? (datum x))
        (evaluate-identifier $scope #'x))
      ((expect type expr)
        (lets
          ($type (evaluate-type $recurse $environment $scope #'type))
          (typed $type
            (evaluate-value $recurse $environment $scope $type #'expr))))
      ((assume type value)
        (typed
          (evaluate-type $recurse $environment $scope #'type)
          (combo
            (eval (datum value) $environment)
            (datum value))))
      ((assume-type value)
        (typed
          any-type
          (combo
            (eval (datum value) $environment)
            (datum value))))
      ((any-lambda (param ...) result)
        (typed any-type
          (combo
            (make-any-lambda
              (map (partial evaluate-type $recurse $environment $scope) (syntaxes param ...))
              (evaluate-type $recurse $environment $scope #'result))
            (syntax->datum $syntax))))
      ((let (binding ...) body)
        (lets
          ($bindings
            (map
              (partial evaluate-binding $recurse $environment $scope)
              (syntaxes binding ...)))
          ($scope
            (fold-left
              (lambda ($scope $binding)
                (lets
                  ((pair $symbol $typed) $binding)
                  (scope+ $scope $symbol
                    (typed-map-value $typed evaluated-bound))))
              $scope
              $bindings))
          ($typed-body
            ($recurse $environment $scope #'body))
          (typed-map-value $typed-body
            (lambda ($evaluated)
              (evaluated-promote $environment $evaluated (length $bindings))))))
      ((lambda (param ...) body)
        (lets
          ($params (syntaxes param ...))
          ($params-length (length $params))
          ($typed-params
            (map
              (partial evaluate-param $recurse $environment $scope)
              $params))
          ($scope
            (fold-left
              (lambda ($scope $typed-param)
                (scope+ $scope
                  (typed-value $typed-param)
                  (typed (typed-type $typed-param) hole)))
              $scope
              $typed-params))
          ($typed-body
            ($recurse $environment $scope #'body))
          (typed
            (make-any-lambda
              (map typed-type $typed-params)
              (typed-type $typed-body))
            (evaluated-promote
              $environment
              (combine-evaluated-list
                $environment
                (list (typed-value $typed-body))
                (lambda ($datums)
                  `(lambda (,@(map typed-value $typed-params))
                    ,@$datums)))
              $params-length))))
      ((fn params ...)
        (lets
          ($params (syntaxes params ...))
          ($typed-lambda (evaluate-lambda $recurse $environment $scope #'fn))
          ($any-lambda (typed-type $typed-lambda))
          ($param-types (any-lambda-params $any-lambda))
          (run
            (unless (= (length $params) (length $param-types))
              (syntax-error $syntax
                (format "invalid param count ~s, expected ~s, in"
                  (length $params)
                  (length $param-types)))))
          ($param-values
            (map
              (partial evaluate-value $recurse $environment $scope)
              $param-types
              $params))
          (typed
            (any-lambda-result $any-lambda)
            (combine-evaluated-list
              $environment
              (append (list (typed-value $typed-lambda)) $param-values)
              (lambda ($datums) `(,@$datums))))))))

  (define (syntax->symbol $syntax)
    (lets
      ($datum (syntax->datum $syntax))
      (if (symbol? $datum)
        $datum
        (syntax-error $syntax "invalid identifier"))))

  (define (evaluate-param $recurse $environment $scope $param)
    (syntax-case $param ()
      ((type id)
        (typed
          (evaluate-type $recurse $environment $scope #'type)
          (syntax->symbol #'id)))
      (other
        (syntax-error #'other "invalid param"))))

  (define (evaluate-binding $recurse $environment $scope $binding)
    (syntax-case $binding ()
      ((id expr)
        (cons
          (syntax->symbol #'id)
          ($recurse $environment $scope #'expr)))
      (other
        (syntax-error #'other "invalid binding"))))
)
