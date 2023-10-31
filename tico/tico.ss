(library (tico tico)
  (export
    scope scope? scope-environment scope-bindings
    typed typed? typed-type typed-value
    phased phased? phased-compiled phased-evaluated
    constant constant? constant-value
    variable variable? variable-index
    hole hole?

    native->item
    literal->item
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

  (define (default-environment)
    (environment `(micascheme)))

  (define (empty-scope)
    (scope (default-environment) (stack)))

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
          (push $items (literal->item $literal))
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
          ((doing) TODO)
          ((apply) TODO)
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
            (scope-native->item $scope $literal))
          $end-fn))
      (lambda ($symbol)
        (list-reader
          (lambda ($list)
            (native-items-reader
              $scope
              (push $items
                (scope-native->item $scope
                  `(,$symbol ,@$list)))
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

  (define (literal->item $literal)
    (switch $literal
      ((boolean? $boolean)
        (type-literal->item (boolean-type) $boolean))
      ((char? $char)
        (type-literal->item (char-type) $char))
      ((number? $number)
        (type-literal->item (number-type) $number))
      ((string? $string)
        (type-literal->item (string-type) $string))
      ((else $other)
        (throw invalid-literal $literal))))

  (define (scope-native->item $scope $native)
    (typed
      (native-type)
      (phased
        $native
        (constant (eval $native (scope-environment $scope))))))

  (define (native->item $native)
    (scope-native->item (empty-scope) $native))

  (define (type-literal->item $type $literal)
    (typed $type
      (phased
        $literal
        (constant $literal))))

  (define (struct-item $symbol $items)
    (typed
      (struct-type $symbol
        (map typed-type $items))
      (phased-tuple
        (filter typed-value $items))))

  (define (phased-tuple $phaseds)
    (and
      (not (null? $phaseds))
      (phased
        (compiled-tuple (map phased-compiled $phaseds))
        (evaluated-tuple (map phased-evaluated $phaseds)))))

  (define (compiled-tuple $compileds)
    (tuple-expression $compileds))

  (define (evaluated-tuple $evaluated)
    (and
      (for-all constant? $evaluated)
      (constant-tuple $evaluated)))

  (define (constant-tuple $constants)
    (constant (tuple-value (map constant-value $constants))))
)
