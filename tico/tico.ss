(library (tico tico)
  (export
    scope scope? scope-environment scope-bindings
    typed typed? typed-type typed-value
    phased phased? phased-compiled phased-evaluated
    constant constant? constant-value
    variable variable? variable-index

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
  (data (variable index))

  (define-syntax-rule (tico-item $body ...)
    (car (ensure single? (tico-items $body ...))))

  (define-syntax-rule (tico-items $body ...)
    (reader-eval
      (items-reader
        (scope
          (environment `(micascheme))
          (stack))
        (stack)
        identity)
      $body ...))

  (define (items-reader $scope $item-stack $end-fn)
    (reader
      (lambda ($literal)
        (items-reader
          $scope
          (push $item-stack (literal->item $literal))
          $end-fn))
      (lambda ($symbol)
        (case $symbol
          ((do)
            (items-reader
              (scope+items $scope (reverse $item-stack))
              (stack)
              (lambda ($do-items)
                (items-reader
                  $scope
                  (reverse $do-items)
                  $end-fn))))
          ((doing) TODO)
          ((apply) TODO)
          ((take)
            (items-reader $scope (stack)
              (lambda ($take-items)
                (items-reader
                  $scope
                  (push-list $item-stack $take-items)
                  $end-fn))))
          (else
            (items-reader $scope (stack)
              (lambda ($struct-items)
                (items-reader
                  $scope
                  (push $item-stack (struct-item $symbol $struct-items))
                  $end-fn))))))
      (lambda ()
        (app $end-fn (reverse $item-stack)))))

  (define (scope+items $scope $items)
    (fold-left scope+item $scope $items))

  (define (scope+item $scope $item)
    (scope
      (scope-environment $scope)
      (push
        (scope-bindings $scope)
        (item->binding $item))))

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
      ((number? $number)
        (type-literal->item (number-type) $number))
      ((string? $string)
        (type-literal->item (string-type) $string))
      ((else $other)
        (throw invalid-literal $literal))))

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
