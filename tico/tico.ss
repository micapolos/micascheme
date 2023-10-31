(library (tico tico)
  (export)
  (import
    (micascheme)
    (tico type)
    (tico value))

  (data (scope environment items))
  (data (item type value))
  (data (thunk datum value))
  (data (constant value))
  (data (variable index))
  (data (hole))
  (data (symbolic symbol value))

  (define (take-reader $scope $items $end-fn)
    (reader
      (lambda ($literal)
        (take-reader
          $scope
          (push $items (literal->item $literal))
          $end-fn))
      (lambda ($symbol)
        (case $symbol
          ((do)
            (take-reader
              (scope+items $scope $items)
              (stack)
              (lambda ($take-items)
                (take-reader
                  $scope
                  $take-items
                  $end-fn))))
          ((doing) TODO)
          ((apply) TODO)
          ((take)
            (take-reader $scope (stack)
              (lambda ($take-items)
                (take-reader
                  $scope
                  (push $items $take-items)
                  $end-fn))))
          ((make)
            (make-reader $scope (stack)
              (lambda ($make-items)
                (take-reader
                  $scope
                  (push $items (struct-item $symbol $top-level-items))
                  $end-fn)))))
          (else
            (take-reader $scope (stack)
              (lambda ($top-level-items)
                (top-level-reader
                  $scope
                  (push $items (struct-item $symbol $top-level-items))
                  $end-fn))))))
      (lambda ()
        (app $end-fn $items))))

  (define (make-reader $scope $items $end-fn)
    (reader
      (lambda ($literal)
        (make-reader
          $scope
          (push $items (literal->item $literal))
          $end-fn))
      (lambda ($symbol)
        (take-reader $scope (stack)
          (lambda ($top-level-items)
            (make-reader
              $scope
              (push $items (struct-item $symbol $top-level-items))
              $end-fn))))
      (lambda ()
        (app $end-fn $items))))

  (define (scope+items $scope $items)
    (fold-left scope+item $scope $items))

  (define (scope+item $scope $item)
    (scope
      (scope-environment $scope)
      (push
        (scope-items $scope)
        (binding-item $item))))

  (define (thunk-item->binding-item $item)
    (item
      (item-type $item)
      (thunk->binding (item-value $item))))

  (define (thunk->binding $thunk)
    (switch (thunk-value $thunk)
      ((constant? $constant) $constant)
      ((variable? _) (hole))))

  (define (literal->item $literal)
    (switch $literal
      ((boolean? $boolean)
        (type-literal->item (boolean-type) $boolean))
      ((number? $number)
        (type-literal->item (number-type) $number))
      ((string? $string)
        (type-literal->item (string-type) $string))
      ((else $other)
        (throw invalid-literal $literal))))

  (define (type-literal->item $type $literal)
    (typed $type
      (thunk $literal
        (constant $literal))))

  (define (struct-item $symbol $items)
    (item
      (struct-type $symbol
        (map item-type $items))
      (tuple-value
        (filter item-value-opt $items))))

  (define (tuple-value $values)
    (value
      (tuple-expression (map value-comptime $values))
      (runtime-tuple (map value-runtime $values))))
)
