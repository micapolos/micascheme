(library (typed evaluate)
  (export evaluate-typed)
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
    (typed hole))

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
          ((else $other)
            $other)))))

  (define (evaluate-value $default? $environment $scope $type $syntax)
    (lets
      ($typed (evaluate-typed $default? $environment $scope $syntax))
      (if (type=? (typed-type $typed) $type)
        (typed-value $typed)
        (syntax-error $syntax
          (format "invalid type ~s, expected ~s, in"
            (typed-type $typed)
            $type)))))

  (define (evaluate-lambda $default? $environment $scope $syntax)
    (lets
      ($typed (evaluate-typed $default? $environment $scope $syntax))
      (switch (typed-type $typed)
        ((any-lambda? _)
          $typed)
        ((else $type)
          (syntax-error $syntax
            (format "invalid type ~s, expected any-lambda, in" $type))))))

  (define (evaluate-type $default? $environment $scope $syntax)
    (switch (evaluate-value $default? $environment $scope any-type $syntax)
      ((thunk? _)
        (syntax-error $syntax "type not constant"))
      ((else $type)
        $type)))

  (define (evaluate-typed $default? $environment $scope $syntax)
    (or
      ($default? $environment $scope $syntax)
      (syntax-case $syntax (assume assume-type any-lambda lambda expect let)
        (x
          (symbol? (datum x))
          (evaluate-identifier $scope #'x))
        ((expect type expr)
          (lets
            ($type (evaluate-type $default? $environment $scope #'type))
            (typed $type
              (evaluate-value $default? $environment $scope $type #'expr))))
        ((assume type value)
          (typed
            (evaluate-type $default? $environment $scope #'type)
            (eval (datum value) $environment)))
        ((assume-type value)
          (typed
            any-type
            (eval (datum value) $environment)))
        ((any-lambda (param ...) result)
          (typed any-type
            (make-any-lambda
              (map (partial evaluate-type $default? $environment $scope) (syntaxes param ...))
              (evaluate-type $default? $environment $scope #'result))))
        ((let (binding ...) body)
          (lets
            ($bindings
              (map
                (partial evaluate-binding $default? $environment $scope)
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
              (evaluate-typed $default? $environment $scope #'body))
            (typed-map-value $typed-body
              (lambda ($evaluated)
                (evaluated-promote $environment $evaluated (length $bindings))))))
        ((lambda (param ...) body)
          (lets
            ($params (syntaxes param ...))
            ($params-length (length $params))
            ($typed-params
              (map
                (partial evaluate-param $default? $environment $scope)
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
              (evaluate-typed $default? $environment $scope #'body))
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
            ($typed-lambda (evaluate-lambda $default? $environment $scope #'fn))
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
                (partial evaluate-value $default? $environment $scope)
                $param-types
                $params))
            (typed
              (any-lambda-result $any-lambda)
              (combine-evaluated-list
                $environment
                (append (list (typed-value $typed-lambda)) $param-values)
                (lambda ($datums) `(,@$datums)))))))))

  (define (syntax->symbol $syntax)
    (lets
      ($datum (syntax->datum $syntax))
      (if (symbol? $datum)
        $datum
        (syntax-error $syntax "invalid identifier"))))

  (define (evaluate-param $default? $environment $scope $param)
    (syntax-case $param ()
      ((type id)
        (typed
          (evaluate-type $default? $environment $scope #'type)
          (syntax->symbol #'id)))
      (other
        (syntax-error #'other "invalid param"))))

  (define (evaluate-binding $default? $environment $scope $binding)
    (syntax-case $binding ()
      ((id expr)
        (cons
          (syntax->symbol #'id)
          (evaluate-typed $default? $environment $scope #'expr)))
      (other
        (syntax-error #'other "invalid binding"))))
)
