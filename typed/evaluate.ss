(library (typed evaluate)
  (export
    scope scope-ref
    hole hole?
    thunk thunk? thunk-max-index thunk-compiled
    compiled compiled compiled-bindings compiled-datum
    evaluate-syntax)
  (import
    (micascheme)
    (evaluator)
    (any)
    (typed lang)
    (typed typed)
    (typed type))

  (data (compiled bindings datum))
  (data (thunk max-index compiled))
  (data hole)

  (define (compiled-value $environment $compiled)
    (evaluate
      (evaluator
        $environment
        (compiled-bindings $compiled))
      (compiled-datum $compiled)))

  (define-rule-syntax (scope (symbol value) ...)
    (stack (cons 'symbol value) ...))

  (define (scope+ $scope $symbol $value)
    (push $scope (cons $symbol $value)))

  (define (scope-ref $scope $symbol)
    (indexed-find
      (lambda ($index $ass)
        (and
          (symbol=? (car $ass) $symbol)
          (cons $index (cdr $ass))))
      $scope))

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

  (define (evaluate-value $environment $scope $type $syntax)
    (lets
      ($typed (evaluate-syntax $environment $scope $syntax))
      (if (type=? (typed-type $typed) $type)
        (typed-value $typed)
        (syntax-error $syntax "invalid type"))))

  (define (evaluate-type $environment $scope $syntax)
    (switch (evaluate-value $environment $scope any-type $syntax)
      ((thunk? _)
        (syntax-error $syntax "type not constant"))
      ((else $type)
        $type)))

  (define (evaluate-syntax $environment $scope $syntax)
    (syntax-case $syntax (assume assume-type any-type any-string any-number any-lambda lambda expect let)
      ((expect type expr)
        (lets
          ($type (evaluate-type $environment $scope #'type))
          (typed $type
            (evaluate-value $environment $scope $type #'expr))))
      ((assume type value)
        (typed
          (evaluate-type $environment $scope #'type)
          (eval (datum value) $environment)))
      ((assume-type value)
        (typed
          any-type
          (eval (datum value) $environment)))
      (any-type
        (typed any-type any-type))
      (any-string
        (typed any-type any-string))
      (any-number
        (typed any-type any-number))
      ((any-lambda (param ...) result)
        (typed any-type
          (make-any-lambda
            (map (partial evaluate-type $environment $scope) (syntaxes param ...))
            (evaluate-type $environment $scope #'result))))
      (x
        (string? (datum x))
        (typed any-string (datum x)))
      (x
        (number? (datum x))
        (typed any-number (datum x)))
      ((let (binding ...) body)
        (lets
          ($bindings
            (map
              (partial evaluate-binding $environment $scope)
              (syntaxes binding ...)))
          ($scope
            (fold-left
              (lambda ($binding)
                (lets
                  ((pair $symbol $typed) $binding)
                  (scope+ $scope $symbol
                    (typed
                      (typed-type $typed)
                      (switch (typed-value $typed)
                        ((thunk? $thunk) hole)
                        ((else $value) $value))))))
              $scope
              $bindings))
          ($typed-body
            (evaluate-syntax $environment $scope #'body))
          (typed
            (typed-type $typed-body)
            (switch (typed-value $typed-body)
              ((thunk? $thunk)
                TODO)
              ((else $value)
                TODO)))))
      ((lambda (param ...) body)
        (lets
          ($params (syntaxes param ...))
          ($params-length (length $params))
          ($typed-params
            (map
              (partial evaluate-param $environment $scope)
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
            (evaluate-syntax $environment $scope #'body))
          (typed
            (make-any-lambda
              (map typed-type $typed-params)
              (typed-type $typed-body))
            (switch (typed-value $typed-body)
              ((thunk? (thunk $max-index $compiled))
                (lets
                  ((compiled $bindings $datum) $compiled)
                  ($datum
                    `(lambda (,@(map typed-value $typed-params))
                      ,$datum))
                  (cond
                    ((< $max-index $params-length)
                      (eval $datum $environment))
                    (else
                      (thunk
                        (- $max-index $params-length)
                        (compiled $bindings $datum))))))
              ((else $value)
                (lets
                  ($tmp (gensym))
                  (
                    (eval
                      `(lambda (,$tmp)
                        (lambda (,@(map typed-value $typed-params))
                          ,$tmp))
                      $environment)
                    $value)))))))
      (x
        (symbol? (datum x))
        (evaluate-identifier $scope #'x))))

  (define (syntax->symbol $syntax)
    (lets
      ($datum (syntax->datum $syntax))
      (if (symbol? $datum)
        $datum
        (syntax-error $syntax "invalid identifier"))))

  (define (evaluate-param $environment $scope $param)
    (syntax-case $param ()
      ((type id)
        (typed
          (evaluate-type $environment $scope #'type)
          (syntax->symbol #'id)))
      (other
        (syntax-error #'other "invalid param"))))

  (define (evaluate-binding $environment $scope $binding)
    (syntax-case $binding ()
      ((id expr)
        (cons
          (syntax->symbol #'id)
          (evaluate-syntax $environment $scope #'expr)))
      (other
        (syntax-error #'other "invalid binding"))))

  (define (value-max-index? $value)
    (switch $value
      ((thunk? $thunk) (thunk-max-index $thunk))
      ((else _) #f)))

  (define (values-max-index? $values)
    (fold-left
      (lambda ($max-index? $value)
        (lets
          ($value-max-index? (value-max-index? $value))
          (if $max-index?
            (if $value-max-index?
              (max $max-index? $value-max-index?)
              $max-index?)
            $value-max-index?)))
      #f
      $values))
)
