(library (tico-3)
  (export
    scope scope? scope-environment scope-bindings
    constant constant? constant-value
    variable variable? variable-index
    hole hole?
    thunk thunk? thunk-runtime thunk-bindings thunk-datum

    compiled compiled? compiled-bindings compiled-value
    compiled-thunk-datum
    compiler compiler-compiled compiler+binding compiler-bind compilers-flatten

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

  (data (compiled bindings value))

  (data (thunk runtime bindings datum))

  (define (compiler-compiled $compiler)
    (app $compiler (stack)))

  (define (compiler $value)
    (lambda ($bindings)
      (compiled $bindings $value)))

  (define (compiler+binding $compiler $binding)
    (lambda ($bindings)
      (app $compiler (push $bindings $binding))))

  (define (compiler-bind $compiler $fn)
    (lambda ($bindings)
      (lets
        ($compiled (app $compiler $bindings))
        (app
          ($fn (compiled-value $compiled))
          (compiled-bindings $compiled)))))

  (define (compilers-flatten $compilers)
    (switch $compilers
      ((null? _) (compiler (list)))
      ((pair? $pair)
        (unpair $pair $compiler $compilers
          (compiler-bind $compiler
            (lambda ($value)
              (compiler-bind (compilers-flatten $compilers)
                (lambda ($values)
                  (compiler (cons $value $values))))))))))

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
      (switch (thunk-runtime $thunk)
        ((constant? $constant) $constant)
        ((variable? $variable) (hole)))))

  (define (syntax->thunk $datum)
    (scope-syntax->thunk
      (environment->scope (environment `(micascheme)))
      $datum))

  (define (scope-syntax->thunk $scope $datum)
    (syntax-case $datum ()
      ((($lambda ($param ...) $body) $arg ...)
        (identifier-named? (syntax $lambda) lambda)
        (lets
          ($arg-thunks
            (map
              (partial scope-syntax->thunk $scope)
              (syntax->list (syntax ($arg ...)))))
          ($arity (length $arg-thunks))
          ($params
            (map syntax->datum
              (map
                (lambda ($param) (ensure identifier? $param))
                (syntax->list (syntax ($param ...))))))
          ($bindings
            (map symbol-thunk->binding $params $arg-thunks))
          ($scope
            (scope+bindings $scope $bindings))
          ($body-thunk
            (scope-syntax->thunk $scope
              (syntax->datum (syntax $body))))
          (switch (thunk-runtime $body-thunk)
            ((constant? $constant)
              (thunk
                $constant
                (thunk-bindings $body-thunk)
                (thunk-datum $body-thunk)))
            ((variable? $variable)
              (lets
                ($index (- (variable-index $variable) $arity))
                (cond
                  ((>= $index 0)
                    (thunk
                      (variable $index)
                      (thunk-bindings $body-thunk)
                      (thunk-datum $body-thunk)))
                  (else
                    (thunk
                      (constant (scope-evaluate $scope (thunk-datum $body-thunk)))
                      (thunk-bindings $body-thunk)
                      (thunk-datum $body-thunk)))))))))
      (($lambda ($param ...) $body)
        (identifier-named? (syntax $lambda) lambda)
        (lets
          ($params
            (map syntax->datum
              (map
                (lambda ($param) (ensure identifier? $param))
                (syntax->list (syntax ($param ...))))))
          ($arity (length $params))
          ($scope
            (fold-left scope+binding $scope
              (map
                (lambda ($param)
                  (cons $param (hole)))
                $params)))
          ($body-thunk
            (scope-syntax->thunk $scope
              (syntax->datum (syntax $body))))
          ($datum
            `(lambda (,@$params)
              ,(thunk-datum $body-thunk)))
          (thunk
            (switch (thunk-runtime $body-thunk)
              ((constant? $constant)
                (constant (evaluate (thunk-bindings $body-thunk) $datum)))
              ((variable? $variable)
                (lets
                  ($index (- (variable-index $variable) $arity))
                  (if (< $index 0)
                    (constant (evaluate (thunk-bindings $body-thunk) $datum))
                    (variable $index)))))
            (thunk-bindings $body-thunk)
            $datum)))
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
          (thunk $value (stack) $symbol)))
      ($other
        (switch (syntax->datum (syntax $other))
          ((boolean? $boolean)
            (thunk (constant $boolean) (stack) $boolean))
          ((number? $number)
            (thunk (constant $number) (stack) $number))
          ((string? $string)
            (thunk (constant $string) (stack) $string))
          ((else _)
            (syntax-error (syntax $other)))))))

  (define (thunk-apply $fn-thunk $arg-thunks)
    (thunk
      (runtime-apply
        (thunk-runtime $fn-thunk)
        (map thunk-runtime $arg-thunks))
      (bindings-apply
        (thunk-bindings $fn-thunk)
        (map thunk-bindings $arg-thunks))
      (datum-apply
        (thunk-datum $fn-thunk)
        (map thunk-datum $arg-thunks))))

  (define (runtime-apply $fn-runtime $arg-runtimes)
    (if (and (constant? $fn-runtime) (for-all constant? $arg-runtimes))
      (constant
        (apply
          (constant-value $fn-runtime)
          (map constant-value $arg-runtimes)))
      (variable
        (apply max
          (map variable-index
            (filter variable? (cons $fn-runtime $arg-runtimes)))))))

  (define (bindings-apply $fn-bindings $arg-bindings-list)
    (fold-left push-all $fn-bindings $arg-bindings-list))

  (define (datum-apply $fn-datum $arg-datums)
    `(,$fn-datum ,@$arg-datums))

  (define (compiled-thunk-datum $compiled)
    `(lets
      ,@(map
        (lambda ($binding) `(,(car $binding) ,(cdr $binding)))
        (compiled-bindings $compiled))
      (thunk-datum (parsed-thunk $compiled))))
)
