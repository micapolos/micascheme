(library (tico-3)
  (export
    scope scope? scope-environment scope-bindings
    constant constant? constant-value
    variable variable? variable-index
    hole hole?
    thunk thunk? thunk-value thunk-datum

    environment->scope
    scope+value scope-value
    syntax->thunk)
  (import
    (micascheme)
    (evaluator))

  (data (constant value))
  (data (variable index))
  (data (hole))
  (data (scope environment bindings))

  (data (thunk value datum))

  (define (environment->scope $environment)
    (scope $environment (stack)))

  (define (scope+value $scope $symbol $value)
    (scope
      (scope-environment $scope)
      (push
        (scope-bindings $scope)
        (cons $symbol $value))))

  (define (scope+binding $scope $binding)
    (scope
      (scope-environment $scope)
      (push (scope-bindings $scope) $binding)))

  (define (scope+bindings $scope $bindings)
    (scope
      (scope-environment $scope)
      (push-list (scope-bindings $scope) $bindings)))

  (define (scope-value $scope $symbol)
    (or
      (indexed-find
        (lambda ($index $binding)
          (and
            (symbol=? $symbol (car $binding))
            (switch (cdr $binding)
              ((constant? $constant) $constant)
              ((hole? _) (variable $index)))))
        (scope-bindings $scope))
      (constant
        (top-level-value $symbol (scope-environment $scope)))))

  (define (scope-evaluate $scope $datum)
    (evaluate
      (evaluator
        (scope-environment $scope)
        (filter
          (lambda ($binding)
            (and
              (constant? (cdr $binding))
              (cons
                (car $binding)
                (constant-value (cdr $binding)))))
          (scope-bindings $scope)))
      $datum))

  (define (symbol-thunk->binding $symbol $thunk)
    (cons $symbol
      (switch (thunk-value $thunk)
        ((constant? $constant) $constant)
        ((variable? _) (hole)))))

  (define (syntax->thunk $syntax)
    (lets
      ($thunk
        (scope-syntax->thunk
          (environment->scope (environment `(micascheme)))
          $syntax))
      (thunk
        (constant-value (ensure constant? (thunk-value $thunk)))
        (thunk-datum $thunk))))

  (define (scope-syntax->thunk $scope $syntax)
    (syntax-case $syntax ()
      (($let (($param $arg) ...) $body)
        (identifier-named? #'$let let)
        (lets
          ($arg-thunks
            (map
              (partial scope-syntax->thunk $scope)
              (syntax->list #'($arg ...))))
          ($arg-datums (map thunk-datum $arg-thunks))
          ($arity (length $arg-thunks))
          ($params
            (map syntax->datum
              (map
                (lambda ($param) (ensure identifier? $param))
                (syntax->list #'($param ...)))))
          ($bindings
            (map symbol-thunk->binding $params $arg-thunks))
          ($scope
            (scope+bindings $scope $bindings))
          ($body-thunk
            (scope-syntax->thunk $scope #'$body))
          ($body-datum (thunk-datum $body-thunk))
          (thunk
            (switch (thunk-value $body-thunk)
              ((constant? $constant) $constant)
              ((variable? $variable)
                (lets
                  ($index (- (variable-index $variable) $arity))
                  (cond
                    ((< $index 0) (constant (scope-evaluate $scope $body-datum)))
                    (else (variable $index))))))
            `(let
              (,@(map
                (lambda ($param $arg-datum) `(,$param ,$arg-datum))
                $params $arg-datums))
              ,$body-datum))))
      (($assert $condition $body)
        (identifier-named? #'$assert assert)
        (lets
          ($condition-thunk (scope-syntax->thunk $scope #'$condition))
          (switch (thunk-value $condition-thunk)
            ((constant? $constant)
              (cond
                ((constant-value $constant)
                  (scope-syntax->thunk $scope #'$body))
                (else
                  (syntax-error #'$condition "assertion failed"))))
            ((variable? $variable)
              (syntax-error #'$condition "not a constant")))))
      (($lambda ($param ...) $body)
        (identifier-named? #'$lambda lambda)
        (lets
          ($params
            (map syntax->datum
              (map
                (lambda ($param) (ensure identifier? $param))
                (syntax->list #'($param ...)))))
          ($arity (length $params))
          ($bindings
            (map
              (lambda ($param) (cons $param (hole)))
              $params))
          ($scope
            (fold-left scope+binding $scope $bindings))
          ($body-thunk (scope-syntax->thunk $scope #'$body))
          (lets
            ($environment (scope-environment $scope))
            ($datum
              `(lambda (,@$params)
                ,(thunk-datum $body-thunk)))
            (thunk
              (switch (thunk-value $body-thunk)
                ((constant? $constant)
                  (lets
                    ($symbol (generate-symbol))
                    (constant
                      (evaluate
                        (evaluator $environment
                          (stack (cons $symbol (constant-value $constant))))
                        `(lambda (,@$params) ,$symbol)))))
                ((variable? $variable)
                  (lets
                    ($index (- (variable-index $variable) $arity))
                    (if (< $index 0)
                      (constant (scope-evaluate $scope $datum))
                      (variable $index)))))
              $datum))))
      (($fn $arg ...)
        (thunk-apply
          (scope-syntax->thunk $scope #'$fn)
          (map
            (partial scope-syntax->thunk $scope)
            (syntax->list #'($arg ...)))))
      ($identifier
        (identifier? #'$identifier)
        (scope-symbol->thunk $scope
          (syntax->datum #'$identifier)))
      ($other
        (switch (syntax->datum #'$other)
          ((boolean? $boolean)
            (literal->thunk $boolean))
          ((number? $number)
            (literal->thunk $number))
          ((string? $string)
            (literal->thunk $string))
          ((else _)
            (syntax-error #'$other))))))

  (define (literal->thunk $literal)
    (thunk (constant $literal) $literal))

  (define (scope-symbol->thunk $scope $symbol)
    (thunk (scope-value $scope $symbol) $symbol))

  (define (thunk-apply $fn-thunk $arg-thunks)
    (thunk
      (value-apply
        (thunk-value $fn-thunk)
        (map thunk-value $arg-thunks))
      (datum-apply
        (thunk-datum $fn-thunk)
        (map thunk-datum $arg-thunks))))

  (define (value-apply $fn-value $arg-values)
    (if (and (constant? $fn-value) (for-all constant? $arg-values))
      (constant
        (apply
          (constant-value $fn-value)
          (map constant-value $arg-values)))
      (variable
        (apply max
          (map variable-index
            (filter variable? (cons $fn-value $arg-values)))))))

  (define (datum-apply $fn-datum $arg-datums)
    `(,$fn-datum ,@$arg-datums))
)
