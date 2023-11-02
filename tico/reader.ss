(library (tico reader)
  (export
    scope scope? scope-environment scope-bindings
    phased phased? phased-compiled phased-evaluated
    constant constant? constant-value
    variable variable? variable-index
    hole hole?

    native-item
    literal-item
    struct-item
    item-get
    item-compile
    type-item
    tico-item tico-items tico-eval

    value-arity->values
    type-value->type
    types-values->types)
  (import
    (micascheme)
    (evaluator)
    (leo reader)
    (leo parser)
    (tico type)
    (tico typed)
    (tico expression)
    (tico value)
    (tico datum))

  (data (scope environment bindings))
  (data (phased compiled evaluated))
  (data (constant value))
  (data (hole))
  (data (variable index))

  (define (native-environment)
    (environment `(micascheme) `(tico type)))

  (define (empty-scope)
    (scope (native-environment) (stack)))

  (define-syntax-rule (tico-item $body ...)
    (car (ensure single? (tico-items $body ...))))

  (define-syntax-rule (tico-items $body ...)
    (reader-eval
      (items-reader
        (empty-scope)
        (stack)
        identity)
      $body ...))

  (define (tico-eval $datum)
    (constant-value
      (phased-evaluated
        (typed-value
          (car
            (ensure single?
              (reader-end
                (reader-read-list
                  (items-reader (empty-scope) (stack) identity)
                  $datum))))))))

  (define (items-reader $scope $items $end-fn)
    (reader
      (lambda ($literal)
        (items-reader
          $scope
          (push $items (literal-item $literal))
          $end-fn))
      (lambda ($symbol)
        (case $symbol
          ((native)
            (native-items-reader $scope (stack)
              (lambda ($native-items)
                (items-reader
                  $scope
                  (push-all $items $native-items)
                  $end-fn))))
          ((typeof)
            (items-reader $scope (stack)
              (lambda ($typeof-items)
                (items-reader
                  $scope
                  (push-all $items (map item->typeof-item $typeof-items))
                  $end-fn))))
          ((type)
            (items-reader $scope (stack)
              (lambda ($type-items)
                (items-reader
                  $scope
                  (push-all $items (map item->type-item $type-items))
                  $end-fn))))
          ((do)
            (lets
              ($arity (length $items))
              ($bindings (items->bindings $items))
              ($let-entries
                (reverse
                  (filter-opts
                    (map
                      (lambda ($item $binding) 
                        (and-lets
                          ($item-phased (typed-value $item))
                          ($binding-phased (typed-value $binding))
                          `(
                            ,(phased-compiled $binding-phased)
                            ,(phased-compiled $item-phased))))
                      $items
                      $bindings))))
              ($do-scope (fold-left scope+binding $scope $bindings))
              (items-reader $do-scope (stack)
                (lambda ($do-items)
                  (lets
                    ($do-item (items->item $do-items))
                    ($do-type (typed-type $do-item))
                    ($do-phased (typed-value $do-item))
                    ($item 
                      (typed
                        (typed-type $do-item)
                        (and $do-phased
                          (lets
                            ($compiled 
                              `(let (,@$let-entries)
                                ,(phased-compiled $do-phased)))
                            (phased $compiled
                              (switch (phased-evaluated $do-phased)
                                ((constant? $constant) $constant)
                                ((variable? $variable)
                                  (lets
                                    ($index (- (variable-index $variable) $arity))
                                    (cond
                                      ((< $index 0) 
                                        (constant
                                          (scope-compiled->evaluate $scope $compiled)))
                                      (else 
                                        (variable $index)))))))))))
                    (items-reader
                      $scope 
                      (stack $item) 
                      $end-fn))))))
          ((get) 
            (items-reader $scope (stack)
              (lambda ($get-items)
                (lets
                  ($get-item (items->item $get-items))
                  ($get-type (item->evaluated-type $get-item))
                  (items-reader
                    $scope
                    (stack
                      (case (length $items)
                        ((0)
                          (scope-get $scope $get-type))
                        ((1)
                          (item-get (car $items) $get-type))
                        (else 
                          (throw get-multiple-items))))
                    $end-fn)))))
          ((doing) TODO)
          ((apply)
            (lets
              ($item (items->item $items))
              ($item-type
                (switch (typed-type $item)
                  ((native-type? $native-type) $native-type)
                  ((arrow? $arrow) $arrow)
                  ((else $other) (throw not-lambda $item))))
              ($item-value (typed-value $item))
              (items-reader $scope (stack)
                (lambda ($apply-items)
                  (lets
                    ($apply-items (reverse $apply-items))
                    ($apply-types (map typed-type $apply-items))
                    ($apply-values (map typed-value $apply-items))
                    ($phased-list (filter-opts (cons $item-value $apply-values)))
                    ($compiled-list (map phased-compiled $phased-list))
                    ($evaluated-list (map phased-evaluated $phased-list))
                    (items-reader
                      $scope
                      (stack
                        (typed
                          (switch $item-type
                            ((native-type? $native-type)
                              $native-type)
                            ((arrow? $arrow)
                              (arrow-result $arrow)))
                          (phased
                            $compiled-list
                            (and
                              (for-all constant? $evaluated-list)
                              (constant
                                (apply
                                  (constant-value (car $evaluated-list))
                                  (map constant-value (cdr $evaluated-list))))))))
                      $end-fn))))))
          ((struct)
            (struct-items-reader $scope (stack)
              (lambda ($struct-items)
                (items-reader
                  $scope
                  (push-all $items $struct-items)
                  $end-fn))))
          ((take)
            (items-reader $scope (stack)
              (lambda ($take-items)
                (items-reader
                  $scope
                  (push-all $items $take-items)
                  $end-fn))))
          ((compile)
            (items-reader $scope (stack)
              (lambda ($compile-items)
                (items-reader
                  $scope
                  (push-all $items
                    (map item-compile $compile-items))
                  $end-fn))))
          (else
            (items-reader $scope (stack)
              (lambda ($struct-items)
                (items-reader
                  $scope
                  (scope-items->items $scope
                    (push $items
                      (struct-item $symbol (reverse $struct-items))))
                  $end-fn))))))
      (lambda ()
        (app $end-fn $items))))

  (define (native-items-reader $scope $items $end-fn)
    (reader
      (lambda ($literal)
        (native-items-reader
          $scope
          (push $items
            (switch $literal
              ((string? $string)
                (scope-native-item $scope (string->native $string)))
              ((else $other)
                (throw not-string $other))))
          $end-fn))
      (lambda ($symbol)
        (throw not-native $symbol))
      (lambda ()
        (app $end-fn $items))))

  (define (struct-items-reader $scope $items $end-fn)
    (reader
      (lambda ($literal)
        (throw not-struct $literal))
      (lambda ($symbol)
        (items-reader $scope (stack)
          (lambda ($struct-items)
            (struct-items-reader
              $scope
              (push $items
                (struct-item $symbol (reverse $struct-items)))
              $end-fn))))
      (lambda ()
        (app $end-fn $items))))

  (define (scope-items->items $scope $items)
    $items)

  (define (item-get $item $pattern)
    (lets
      ($type (typed-type $item))
      ($value (typed-value $item))
      (switch $type
        ((struct? $struct)
          (lets
            ($fields (struct-fields $struct))
            ($arity (types-arity $fields))
            ($indexed-type (indexed-type-matching $fields $pattern))
            (cond
              ($indexed-type
                (lets
                  ($type (indexed-value $indexed-type))
                  ($index (indexed-index $indexed-type))
                  (typed $type
                    (and $index
                      (phased
                        (tuple-ref-expression $arity (phased-compiled $value) $index)
                        (switch (phased-evaluated $value)
                          ((constant? $constant)
                            (constant
                              (tuple-ref-value
                                $arity
                                (constant-value $constant)
                                $index)))
                          ((variable? $variable) $variable)))))))
              (else
                (throw not-found $item $pattern)))))
        ((else $other)
          (throw not-struct $other)))))

  (define (scope-get $scope $pattern)
    (bindings-get (scope-bindings $scope) $pattern))

  (define (bindings-get $bindings $pattern)
    (indexed-find
      (lambda ($index $binding)
        (binding-type-index->item-opt $binding $pattern $index))
      $bindings))

  (define (binding-type-index->item-opt $binding $type $index)
    (and
      (type-matches? $type (typed-type $binding))
      (typed $type
        (lets
          ($phased (typed-value $binding))
          (and $phased
            (phased
              (phased-compiled $phased)
              (switch (phased-evaluated $phased)
                ((constant? $constant) $constant)
                ((hole? $hole) (variable $index)))))))))

  (define (scope-compiled->evaluate $scope $compiled)
    TODO)

  (define (scope+items $scope $items)
    (fold-left scope+binding $scope
      (map item->binding (reverse $items))))

  (define (scope+binding $scope $binding)
    (scope
      (scope-environment $scope)
      (push (scope-bindings $scope) $binding)))

  (define (item->param $item)
    (lets
      ($value (typed-value $item))
      (and $value (generate-symbol))))

  (define (items->params $items)
    (filter item->param $items))

  (define (items->bindings $items)
    (reverse (map item->binding (reverse $items))))

  (define (item->binding $item)
    (typed
      (typed-type $item)
      (opt-lift phased->binding (typed-value $item))))

  (define (phased->binding $phased)
    (phased
      (generate-symbol)
      (switch (phased-evaluated $phased)
        ((constant? $constant) $constant)
        ((variable? _) (hole)))))

  (define (literal-item $literal)
    (switch $literal
      ((boolean? $boolean)
        (type-literal-item (boolean-type) $boolean))
      ((char? $char)
        (type-literal-item (char-type) $char))
      ((number? $number)
        (type-literal-item (number-type) $number))
      ((string? $string)
        (type-literal-item (string-type) $string))
      ((else $other)
        (throw invalid-literal $literal))))

  (define (item-compile $item)
    (typed
      (typed-type $item)
      (opt-lift phased-compile (typed-value $item))))

  (define (phased-compile $phased)
    (switch (phased-evaluated $phased)
      ((constant? $constant)
        (phased
          (value->datum (constant-value $constant))
          $constant))
      ((else $other)
        (throw not-constant))))

  (define (scope-native-item $scope $native)
    (typed
      (native-type)
      (phased
        $native
        (constant
          (eval $native (scope-environment $scope))))))

  (define (native-item $native)
    (scope-native-item (empty-scope) $native))

  (define (type-literal-item $type $literal)
    (typed $type
      (phased
        $literal
        (constant $literal))))

  (define (struct-item $symbol $items)
    (typed
      (struct $symbol
        (map typed-type $items))
      (phased-tuple
        (filter-opts (map typed-value $items)))))

  (define (phased-tuple $phased-list)
    (and
      (not (null? $phased-list))
      (phased
        (compiled-tuple (map phased-compiled $phased-list))
        (evaluated-tuple (map phased-evaluated $phased-list)))))

  (define (compiled-tuple $compiled-list)
    (tuple-expression $compiled-list))

  (define (evaluated-tuple $evaluated-list)
    (and
      (for-all constant? $evaluated-list)
      (constant-tuple $evaluated-list)))

  (define (constant-tuple $constants)
    (constant (tuple-value (map constant-value $constants))))

  (define (items->item $items)
    (or
      (single $items)
      (throw not-item $items)))

  (define (string->native $string)
    (lets
      ($port (open-input-string $string))
      (switch (read $port)
        ((eof-object? _)
          (throw not-datum $string))
        ((else $datum)
          (switch (read $port)
            ((eof-object? _) $datum)
            ((else _) (throw not-datum $string)))))))

  (define (item->typeof-item $item)
    (type-item (typed-type $item)))

  (define (item->type-item $item)
    (type-item (item->evaluated-type $item)))

  (define (type-item $type)
    (typed
      (type-type)
      (phased
        (value->datum $type)
        (constant $type))))

  (define (item->evaluated-type $item)
    (lets
      ($type (typed-type $item))
      ($value (typed-value $item))
      (or
        (and $value (type-phased->type $type $value))
        $type)))

  (define (type-phased->type $type $phased)
    (switch (phased-evaluated $phased)
      ((constant? $constant)
        (type-value->type $type (constant-value $constant)))
      ((variable? _)
        (throw type-phased->type $type $phased))))

  (define (type-value->type $type $value)
    (switch $type
      ((value-type? $value-type) 
        (value-type-value $value-type))
      ((native-type? _)
        $value)
      ((struct? $struct)
        (struct
          (struct-name $struct)
          (lets
            ($fields (struct-fields $struct))
            ($arity (types-arity $fields))
            ($values (value-arity->values $value $arity))
            (types-values->types $fields $values))))
      ((else $other)
        (throw type-value->type $other))))

  (define (types-values->types $types $values)
    (switch $types
      ((null? _) (list))
      ((pair? $pair)
        (unpair $pair $type $types
          (cond
            ((type-dynamic? $type) 
              (cons 
                (type-value->type $type (car $values))
                (types-values->types $types (cdr $values))))
            (else 
              (cons $type
                (types-values->types $types $values))))))))

  (define (value-arity->values $value $arity)
    (lets
      ($symbol (generate-symbol))
      (evaluate
        (evaluator
          (native-environment)
          (stack (cons $symbol $value)))
        `(list
          ,@(map 
            (lambda ($index)
              (tuple-ref-expression $arity $symbol $index))
            (indices $arity))))))
)
