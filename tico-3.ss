(library (tico-3)
  (export
    scope scope? scope-environment scope-bindings
    constant constant? constant-value
    variable variable? variable-index
    hole hole?

    environment->scope
    scope+value scope-value)
  (import (micascheme))

  (data (constant value))
  (data (variable index))
  (data (hole))
  (data (thunk runtime bindings datum))
  (data (scope environment bindings))

  (define (environment->scope $environment)
    (scope $environment (stack)))

  (define (scope+value $scope $symbol $value)
    (scope
      (scope-environment $scope)
      (push
        (scope-bindings $scope)
        (cons $symbol $value))))

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

  ; (define (parse $scope $datum)
  ;   (syntax-case $datum ()
  ;     (($lambda ($param ...) $body)
  ;       (identifier-named? (syntax $lambda) lambda)
  ;       (lets
  ;         ($params (map syntax->datum (syntax->list (syntax ($param ...)))))
  ;         (do
  ;           (unless
  ;             (for-all symbol? $params)
  ;             (syntax-error
  ;               (syntax ($lambda ($param ...) $body))
  ;               "invalid param names")))
  ;         ($bindings (push-list $bindings (map (lambda ($param) (cons $param (hole))) $params)))
  ;         ($body-thunk (parse $bindings (syntax $body)))
  ;         ($body-runtime (thunk-runtime $body-thunk))
  ;         ($datum `(lambda (,@$params) ,(thunk-datum $body-thunk)))
  ;         (switch (thunk-runtime $body-thunk)
  ;           ((constant? $constant)
  ;             (thunk
  ;               (constant
  ;                 (bindings-evaluate
  ;                   (push $bindings (thunk-bindings $bindings))
  ;                   $datum))
  ;               (thunk-datum $body-thunk)
  ;               $datum))
  ;           ((variable? $variable)
  ;             (lets
  ;               ($index (max (- (variable-index $variable) (length $param)) 0))
  ;               (thunk
  ;                 (if (zero? $index)
  ;                   (constant
  ;                     (bindings-evaluate
  ;                       (push $bindings (thunk-bindings $bindings))
  ;                       $datum))
  ;                   (variable $index))
  ;                 (thunk-datum $body-thunk)
  ;                 $datum))))))
  ;     (($fn $arg ...)
  ;       (lets
  ;         ($arg-thunks (map (partial parse $bindings) (syntax->list (syntax $arg ...))))
  ;         ($arg-runtimes (map thunk-runtime $thunks))
  ;         ($fn-thunk
  ;           (syntax-case (syntax $fn) ()
  ;             (($lambda ($params ...) $body)
  ;               (identifier-named? (syntax $lambda) lambda)
  ;               )
  ;             (else
  ;               )))
  ;         ($fn-runtime (thunk-runtime $fn-thunk))
  ;         (if (and (constant? $fn-runtime) (for-all constant? $arg-runtimes))
  ;           (thunk
  ;             (constant (apply constant-value $fn-runtime (map constant-value $arg-runtimes)))
  ;             (stack)
  ;             `(
  ;               ,(thunk-datum $fn-thunk)
  ;               ,@(map thunk-datum $arg-thunks)))
  ;           (thunk
  ;             (variable
  ;               (apply max
  ;                 (cons
  ;                   (variable-index (thunk-variable $fn-runtime))
  ;                   (map variable-index $arg-runtimes))))
  ;             (stack)
  ;             `(
  ;               ,(thunk-datum $fn-thunk)
  ;               ,@(map thunk-datum $arg-thunks))))))
  ;     ($identifier
  ;       (identifier? (syntax $identifier))
  ;       (lets
  ;         ($symbol (syntax->datum (syntax $identifier)))
  ;         ($found
  ;           (indexed-find
  ;             (lambda ($index $binding)
  ;               (and
  ;                 (symbol=? $symbol (car $binding))
  ;                 (indexed (cdr $binding) $index)))
  ;             $bindings))
  ;         (switch $found
  ;           ((indexed? $indexed)
  ;             (switch (indexed-value $indexed)
  ;               ((constant? $constant)
  ;                 (thunk
  ;                   $constant
  ;                   (stack)
  ;                   $symbol))
  ;               ((hole? _)
  ;                 (thunk
  ;                   (variable (indexed-index $index))
  ;                   (stack)
  ;                   $symbol))))
  ;           (else
  ;             (syntax-error (syntax $identifier) "unbound"))))))
  ;     ($other
  ;       (switch (syntax->datum (syntax $other))
  ;         ((boolean $boolean)
  ;           (thunk (constant $boolean $boolean) (stack) $boolean))
  ;         ((number? $number)
  ;           (thunk (constant $number $number) (stack) $number))
  ;         ((string? $string)
  ;           (thunk (constant $string $string) (stack) $string))
  ;         ((else _)
  ;           (syntax-error (syntax $other)))))))
)
