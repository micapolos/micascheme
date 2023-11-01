(library (tico tico)
  (export
    scope scope? scope-environment scope-bindings
    typed typed? typed-type typed-value
    phased phased? phased-compiled phased-evaluated
    constant constant? constant-value
    variable variable? variable-index
    hole hole?

    native-item
    literal-item
    struct-item
    tico-item tico-items)
  (import
    (micascheme)
    (leo reader)
    (tico type)
    (tico expression)
    (tico value))

  (data (scope environment bindings))
  (data (typed type value))
  (data (phased compiled evaluated))
  (data (constant value))
  (data (hole))
  (data (variable index))

  (define (native-environment)
    (environment `(micascheme)))

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
          ((do)
            (items-reader
              (scope+items $scope $items)
              (stack)
              (lambda ($do-items)
                (items-reader $scope $do-items $end-fn))))
          ((get) TODO)
          ((doing) TODO)
          ((apply)
            (lets
              ($item (items->item $items))
              ($item-type
                (switch (typed-type $item)
                  ((native-type? $native-type) $native-type)
                  ((lambda-type? $lambda-type) $lambda-type)
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
                            ((lambda-type? $lambda-type)
                              (lambda-type-result $lambda-type)))
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

  (define (bindings-type->item $bindings $type)
    (indexed-find
      (lambda ($index $binding)
        (binding-type-index->item $binding $type $index))
      $bindings))

  (define (binding-type-index->item $binding $type $index)
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

  (define (scope+items $scope $items)
    (fold-left scope+binding $scope
      (map item->binding (reverse $items))))

  (define (scope+binding $scope $binding)
    (scope
      (scope-environment $scope)
      (push (scope-bindings $scope) $binding)))

  (define (item->binding $item)
    (typed
      (typed-type $item)
      (phased->binding (typed-value $item))))

  (define (phased->binding $phased)
    (switch (phased-evaluated $phased)
      ((constant? $constant) $constant)
      ((variable? _) #f)))

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
      (struct-type $symbol
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
)
