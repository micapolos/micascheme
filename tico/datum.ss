(library (tico datum)
  (export
    test-datum
    test-parameter-datum
    literal->datum
    datum-application
    datum-values-application
    generate-datum-params
    datum-abstraction
    generate-datum-abstraction
    lets-datum
    let-datum
    datum-tuple
    datum-args
    tuple-ref-datum
    value->datum
    datum->value
    bindings-datum->value
    datum-struct
    datum-ref
    string->read-datum
    let-values-entry-datum
    let-values-datum
    datum-definition-let-entry
    datum-definitions-let-entries
    packet-datum
    values-datum)
  (import
    (micascheme)
    (tico type)
    (tico definition)
    (tico packet)
    (tico tuple)
    (evaluator))

  (define-syntax-rule (test-datum $name)
    (quote (quote $name)))

  (define-syntax-rule (test-parameter-datum $name)
    (quote $name))

  (define (literal->datum $literal)
    (switch $literal
      ((boolean? $boolean) $boolean)
      ((number? $number) $number)
      ((string? $string) $string)
      ((char? $char) $char)
      ((symbol? $symbol) `(quote ,$symbol))
      ((else $other) (throw literal->datum $literal))))

  (define (generate-datum-abstraction $arity $fn)
    (lets
      ($params (generate-datum-params $arity))
      (datum-abstraction $params ($fn $params))))

  (define (generate-datum-params $arity)
    (generate-symbols $arity))

  (define (datum-abstraction $params $body)
    `(lambda (,@$params) ,$body))

  (define (datum-application $target $args)
    `(,$target ,@$args))

  (define (datum-values-application $target $args)
    `(values-app ,$target ,@$args))

  (define (datum-tuple $items)
    `(tuple ,@$items))

  (define (tuple-ref-datum $arity $tuple $index)
    `(tuple-ref ,$arity ,$tuple ,$index))

  (define (datum-args $datums) $datums)

  (define (datum-struct $name $field-datums)
    (datum-tuple $field-datums))

  (define (datum-ref $arity $target $index)
    `(tuple-ref ,$arity ,$target ,$index))

  (define (lets-datum $declarations $body)
    (switch $declarations
      ((null? _) $body)
      ((pair? $pair) `(lets ,@$pair ,$body))))

  (define (let-datum $declarations $body)
    (switch $declarations
      ((null? _) $body)
      ((pair? $pair) `(let (,@$pair) ,$body))))

  (define datum-environment
    (environment
      '(micascheme)
      '(tico type)
      '(tico tuple)))

  (define (datum->value $datum)
    (bindings-datum->value (stack) $datum))

  (define (bindings-datum->value $bindings $datum)
    (evaluate
      (evaluator datum-environment $bindings)
      $datum))

  (define (let-values-entry-datum $param-datums $arg-datums)
    `((,@$param-datums) (values ,@$arg-datums)))

  (define (let-values-datum $entries $body)
    `(let-values (,@$entries) ,$body))

  (define (value->datum $value)
    (switch $value
      ((null? $null) $null)
      ((symbol? $symbol) `(quote ,$symbol))
      ((boolean? $boolean) $boolean)
      ((number? $number) $number)
      ((string? $string) $string)
      ((char? $char) $char)
      ((pair? $pair)
        `(cons
          ,(value->datum (car $pair))
          ,(value->datum (cdr $pair))))
      ((vector? $vector)
        `(vector ,@(map value->datum (vector->list $vector))))
      ((type-type? _)
        `(type-type))
      ((any-type? _)
        `(any-type))
      ((unchecked-type? _)
        `(unchecked-type))
      ((value-type? $value-type)
        `(value-type ,(value->datum (value-type-value $value-type))))
      ((struct? $struct)
        `(struct
          ,(value->datum (struct-name $struct))
          (list ,@(map value->datum (struct-fields $struct)))))
      ((arrow? $arrow)
        `(arrow
          (list ,@(map value->datum (arrow-params $arrow)))
          (list ,@(map value->datum (arrow-results $arrow)))))
      ((property? $property)
        `(property
          ,(value->datum (property-param $property))
          ,(value->datum (property-body $property))))
      ((else $other)
        (throw value->datum $value))))

  (define (string->read-datum $string)
    (lets
      ($port (open-input-string $string))
      (switch (read $port)
        ((eof-object? _)
          (throw not-datum $string))
        ((else $datum)
          (switch (read $port)
            ((eof-object? _) $datum)
            ((else _) (throw not-datum $string)))))))

  (define (datum-definition-let-entry $datum-definition)
    `(
      ,(definition-key $datum-definition)
      ,(definition-value $datum-definition)))

  (define (datum-definitions-let-entries $datum-definitions)
    `(,@(map datum-definition-let-entry $datum-definitions)))

  (define (packet-datum $packet)
    (lets
      ($definitions (packet-definitions $packet))
      ($items (packet-items $packet))
      ($items-datum
        (case (length $items)
          ((0) '(void))
          ((1) (car $items))
          (else `(values ,@$items))))
      (switch $definitions
        ((null? _) $items-datum)
        ((else $other)
          `(let
            ,(datum-definitions-let-entries $other)
            ,$items-datum)))))

  (define (values-datum $datums)
    (case (length $datums)
      ((1) (car $datums))
      (else `(values ,@$datums))))
)
