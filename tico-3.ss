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
        ((variable? $variable) (hole)))))

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
      ; ((($lambda ($param ...) $body) $arg ...)
      ;   (identifier-named? (syntax $lambda) lambda)
      ;   (lets
      ;     ($arg-thunks
      ;       (map
      ;         (partial scope-syntax->thunk $scope)
      ;         (syntax->list (syntax ($arg ...)))))
      ;     ($arity (length $arg-thunks))
      ;     ($params
      ;       (map syntax->datum
      ;         (map
      ;           (lambda ($param) (ensure identifier? $param))
      ;           (syntax->list (syntax ($param ...))))))
      ;     ($bindings
      ;       (map symbol-thunk->binding $params $arg-thunks))
      ;     ($scope
      ;       (scope+bindings $scope $bindings))
      ;     ($body-thunk
      ;       (scope-syntax->thunk $scope
      ;         (syntax->datum (syntax $body))))
      ;     (switch (thunk-runtime $body-thunk)
      ;       ((constant? $constant)
      ;         (thunk
      ;           $constant
      ;           (thunk-bindings $body-thunk)
      ;           (thunk-datum $body-thunk)))
      ;       ((variable? $variable)
      ;         (lets
      ;           ($index (- (variable-index $variable) $arity))
      ;           (cond
      ;             ((>= $index 0)
      ;               (thunk
      ;                 (variable $index)
      ;                 (thunk-bindings $body-thunk)
      ;                 (thunk-datum $body-thunk)))
      ;             (else
      ;               (thunk
      ;                 (constant (scope-evaluate $scope (thunk-datum $body-thunk)))
      ;                 (thunk-bindings $body-thunk)
      ;                 (thunk-datum $body-thunk)))))))))
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
        (identifier-named? (syntax $lambda) lambda)
        (lets
          ($params
            (map syntax->datum
              (map
                (lambda ($param) (ensure identifier? $param))
                (syntax->list (syntax ($param ...))))))
          ($arity (length $params))
          ($bindings
            (map
              (lambda ($param) (cons $param (hole)))
              $params))
          ($scope
            (fold-left scope+binding $scope $bindings))
          ($body-thunk (scope-syntax->thunk $scope (syntax $body)))
          (lets
            ($environment (scope-environment $scope))
            ($datum
              `(lambda (,@$params)
                ,(thunk-datum $body-thunk)))
            (thunk
              (switch (thunk-value $body-thunk)
                ((constant? $constant)
                  (constant (scope-evaluate $scope $datum)))
                ((variable? $variable)
                  (lets
                    ($index (- (variable-index $variable) $arity))
                    (if (< $index 0)
                      (constant (scope-evaluate $scope $datum))
                      (variable $index)))))
              $datum))))
      (($fn $arg ...)
        (thunk-apply
          (scope-syntax->thunk $scope (syntax $fn))
          (map
            (partial scope-syntax->thunk $scope)
            (syntax->list (syntax ($arg ...))))))
      ($identifier
        (identifier? (syntax $identifier))
        (lets
          ($symbol (syntax->datum (syntax $identifier)))
          ($value (scope-value $scope $symbol))
          (thunk $value $symbol)))
      ($other
        (switch (syntax->datum (syntax $other))
          ((boolean? $boolean)
            (thunk (constant $boolean) $boolean))
          ((number? $number)
            (thunk (constant $number) $number))
          ((string? $string)
            (thunk (constant $string) $string))
          ((else _)
            (syntax-error (syntax $other)))))))

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
