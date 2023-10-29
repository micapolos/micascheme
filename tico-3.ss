(library (tico-3)
  (export
    scope scope? scope-environment scope-bindings
    constant constant? constant-value
    variable variable? variable-index
    hole hole?
    thunk thunk? thunk-value thunk-datum

    compiled compiled? compiled-bindings compiled-value
    compiler compiler-compiled compiler+binding compiler-bind compilers-flatten

    environment->scope
    scope+value scope-value
    syntax->thunk
    thunk-compiler->symbolize)
  (import
    (micascheme)
    (evaluator))

  (data (constant value))
  (data (variable index))
  (data (hole))
  (data (scope environment bindings))

  (data (compiled bindings value))

  (data (thunk value datum))

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
      (switch (thunk-value $thunk)
        ((constant? $constant) $constant)
        ((variable? $variable) (hole)))))

  (define (syntax->thunk $syntax)
    (compiled-thunk->thunk
      (compiler-compiled
        (syntax->thunk-compiler $syntax))))

  (define (syntax->thunk-compiler $syntax)
    (scope-syntax->thunk-compiler
      (environment->scope (environment `(micascheme)))
      $syntax))

  (define (scope-syntax->thunk-compiler $scope $syntax)
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
        (compiler-bind
          (scope-syntax->thunk-compiler $scope #'$condition)
          (lambda ($condition-thunk)
            (switch (thunk-value $condition-thunk)
              ((constant? $constant)
                (cond
                  ((constant-value $constant)
                    (scope-syntax->thunk-compiler $scope #'$body))
                  (else
                    (syntax-error #'$condition "assertion failed"))))
              ((variable? $variable)
                (syntax-error #'$condition "not a constant"))))))
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
          (compiler-bind
            (thunk-compiler->symbolize
              (scope-syntax->thunk-compiler $scope (syntax $body)))
            (lambda ($body-thunk)
              (lets
                ($environment (scope-environment $scope))
                ($datum
                  `(lambda (,@$params)
                    ,(thunk-datum $body-thunk)))
                (switch (thunk-value $body-thunk)
                  ((constant? $constant)
                    (environment-datum->thunk-compiler $environment $datum))
                  ((variable? $variable)
                    (lets
                      ($index (- (variable-index $variable) $arity))
                      (if (< $index 0)
                        (environment-datum->thunk-compiler $environment $datum)
                        (compiler (thunk (variable $index) $datum)))))))))))
      (($fn $arg ...)
        (thunk-compiler-apply
          (scope-syntax->thunk-compiler $scope (syntax $fn))
          (map
            (partial scope-syntax->thunk-compiler $scope)
            (syntax->list (syntax ($arg ...))))))
      ($identifier
        (identifier? (syntax $identifier))
        (lets
          ($symbol (syntax->datum (syntax $identifier)))
          ($value (scope-value $scope $symbol))
          (compiler (thunk $value $symbol))))
      ($other
        (switch (syntax->datum (syntax $other))
          ((boolean? $boolean)
            (compiler (thunk (constant $boolean) $boolean)))
          ((number? $number)
            (compiler (thunk (constant $number) $number)))
          ((string? $string)
            (compiler (thunk (constant $string) $string)))
          ((else _)
            (syntax-error (syntax $other)))))))

  (define (thunk-compiler->symbolize $thunk-compiler)
    (compiler-bind $thunk-compiler
      (lambda ($thunk)
        (switch (thunk-value $thunk)
          ((variable? _)
            (compiler $thunk))
          ((constant? $constant)
            (switch (thunk-datum $thunk)
              ((symbol? _) (compiler $thunk))
              ((boolean? _) (compiler $thunk))
              ((number? _) (compiler $thunk))
              ((string? _) (compiler $thunk))
              ((else $datum)
                (lets
                  ($symbol (generate-symbol))
                  (compiler+binding
                    (compiler (thunk $constant $symbol))
                    (cons $symbol
                      (thunk (constant-value $constant) $datum)))))))))))

  (define (environment-datum->thunk-compiler $environment $datum)
    (lambda ($bindings)
      (compiled $bindings
        (thunk
          (constant
            (evaluate
              (evaluator
                $environment
                (map
                  (lambda ($binding)
                    (cons
                      (car $binding)
                      (thunk-value (cdr $binding))))
                  $bindings))
              $datum))
          $datum))))

  (define (thunk-compiler-apply $fn-thunk-compiler $arg-thunk-compilers)
    (compiler-bind (thunk-compiler->symbolize $fn-thunk-compiler)
      (lambda ($fn-thunk)
        (compiler-bind (compilers-flatten (map thunk-compiler->symbolize $arg-thunk-compilers))
          (lambda ($arg-thunks)
            (compiler (thunk-apply $fn-thunk $arg-thunks)))))))

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

  (define (compiled-thunk->thunk $compiled)
    (lets
      ($bindings (compiled-bindings $compiled))
      ($thunk (compiled-value $compiled))
      ($datum (thunk-datum $thunk))
      (thunk
        (constant-value (ensure constant? (thunk-value $thunk)))
        (cond
          ((null? $bindings) $datum)
          (else
            `(lets
              ,@(reverse
                (map
                  (lambda ($binding)
                    `(
                      ,(car $binding)
                      ,(thunk-datum (cdr $binding))))
                  (compiled-bindings $compiled)))
              ,(thunk-datum $thunk)))))))
)
