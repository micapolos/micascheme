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
    (scope-syntax->thunk
      (environment->scope (environment `(micascheme)))
      $syntax))

  (define (scope-syntax->thunk $scope $syntax)
    (syntax-case $syntax ()
      ($identifier
        (identifier? #'$identifier)
        (scope-symbol->thunk $scope
          (syntax->datum #'$identifier)))
      (($if $cond $then $else)
        (identifier-named? #'$if if)
        (thunk-if
          (scope-syntax->thunk $scope #'$cond)
          (lambda () (scope-syntax->thunk $scope #'$then))
          (lambda () (scope-syntax->thunk $scope #'$else))))
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
      (($compile-time $body)
        (identifier-named? #'$compile-time compile-time)
        (switch (thunk-value (scope-syntax->thunk $scope #'$body))
          ((constant? $constant)
            (thunk $constant (value->datum (constant-value $constant))))
          ((variable? _)
            (syntax-error #'$body "not compile-time"))))
      (($testing $condition $body)
        (identifier-named? #'$testing testing)
        (lets
          ($condition-thunk (scope-syntax->thunk $scope #'$condition))
          (switch (thunk-value $condition-thunk)
            ((constant? $constant)
              (case (constant-value $constant)
                ((#t) (scope-syntax->thunk $scope #'$body))
                ((#f) (syntax-error #'$condition "test failed"))
                (else (syntax-error #'$condition "not a boolean"))))
            ((variable? $variable)
              (syntax-error #'$condition "not a constant")))))
      (($fn $arg ...)
        (thunk-apply
          (scope-syntax->thunk $scope #'$fn)
          (map
            (partial scope-syntax->thunk $scope)
            (syntax->list #'($arg ...)))))
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

  (define (value->datum $value)
    (switch $value
      ((null? _) `())
      ((boolean? $boolean) $boolean)
      ((number? $number) $number)
      ((string? $string) $string)
      ((pair? $pair)
        `(cons
          ,(value->datum (car $pair))
          ,(value->datum (cdr $pair))))
      ((else $other)
        (throw value->datum $other))))

  (define (datum-apply $fn-datum $arg-datums)
    `(,$fn-datum ,@$arg-datums))

  (define (thunk-if $cond-thunk $then-thunk-fn $else-thunk-fn)
    (switch (thunk-value $cond-thunk)
      ((constant? $constant)
        (if (constant-value $constant)
          (app $then-thunk-fn)
          (app $else-thunk-fn)))
      ((variable? $variable)
        (variable-thunk-if
          $cond-thunk
          (app $then-thunk-fn)
          (app $else-thunk-fn)))))

  (define (variable-thunk-if $cond-thunk $then-thunk $else-thunk)
    (thunk
      (variable
        (apply max
          (map variable-index
            (filter variable?
              (list
                (thunk-value $cond-thunk)
                (thunk-value $then-thunk)
                (thunk-value $else-thunk))))))
      (datum-if
        (thunk-datum $cond-thunk)
        (thunk-datum $then-thunk)
        (thunk-datum $else-thunk))))

  (define (datum-if $cond-datum $then-datum $else-datum)
    `(if ,$cond-datum ,$then-datum ,$else-datum))
)
