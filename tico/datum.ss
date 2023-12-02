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
    datum-parameter
    datum-parameters
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
    values-datum
    argument-datum
    arguments-lets-datum
    datum-arguments-application
    datum-arguments-tuple)
  (import
    (micascheme)
    (tico argument)
    (tico arity)
    (tico type)
    (tico definition)
    (tico packet)
    (tico tuple)
    (evaluator))

  (define-syntax-rule (test-datum $name)
    (quote (quote $name)))

  (define-syntax-rule (test-parameter-datum $name)
    (quote $name))

  (function (literal->datum $literal)
    (switch $literal
      ((boolean? $boolean) $boolean)
      ((number? $number) $number)
      ((string? $string) $string)
      ((char? $char) $char)
      ((symbol? $symbol) `(quote ,$symbol))
      ((else $other) (throw literal->datum $literal))))

  (function (datum-parameter $arity)
    (switch (arity-value $arity)
      ((one? _) (generate-symbol))
      ((else $count) `(values ,@(generate-symbols $count)))))

  (function (datum-parameters $arity)
    (generate-symbols (arity-value $arity)))

  (function (generate-datum-abstraction $arity $fn)
    (lets
      ($params (generate-datum-params $arity))
      (datum-abstraction $params ($fn $params))))

  (function (generate-datum-params $arity)
    (generate-symbols $arity))

  (function (datum-abstraction $params $body)
    `(lambda (,@$params) ,$body))

  (function (datum-application $target $args)
    `(,$target ,@$args))

  (function (datum-values-application $target $args)
    `(values-app ,$target ,@$args))

  (function (datum-tuple $items)
    `(tuple ,@$items))

  (function (tuple-ref-datum $arity $tuple $index)
    `(tuple-ref ,$arity ,$tuple ,$index))

  (function (datum-args $datums) $datums)

  (function (datum-struct $name $field-datums)
    (datum-tuple $field-datums))

  (function (datum-ref $arity $target $index)
    `(tuple-ref ,$arity ,$target ,$index))

  (function (lets-datum $declarations $body)
    (switch $declarations
      ((null? _) $body)
      ((pair? $pair) `(lets ,@$pair ,$body))))

  (function (let-datum $declarations $body)
    (switch $declarations
      ((null? _) $body)
      ((pair? $pair) `(let (,@$pair) ,$body))))

  (define datum-environment
    (environment
      '(micascheme)
      '(tico type)
      '(tico tuple)))

  (function (datum->value $datum)
    (bindings-datum->value (stack) $datum))

  (function (bindings-datum->value $bindings $datum)
    (evaluate
      (evaluator datum-environment $bindings)
      $datum))

  (function (let-values-entry-datum $param-datums $arg-datums)
    `((,@$param-datums) (values ,@$arg-datums)))

  (function (let-values-datum $entries $body)
    `(let-values (,@$entries) ,$body))

  (function (value->datum $value)
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

  (function (string->read-datum $string)
    (lets
      ($port (open-input-string $string))
      (switch (read $port)
        ((eof-object? _)
          (throw not-datum $string))
        ((else $datum)
          (switch (read $port)
            ((eof-object? _) $datum)
            ((else _) (throw not-datum $string)))))))

  (function (datum-definition-let-entry $datum-definition)
    `(
      ,(definition-key $datum-definition)
      ,(definition-value $datum-definition)))

  (function (datum-definitions-let-entries $datum-definitions)
    `(,@(map datum-definition-let-entry $datum-definitions)))

  (function (packet-datum $packet)
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

  (function (values-datum $datums)
    (case (length $datums)
      ((1) (car $datums))
      (else `(values ,@$datums))))

  (function (argument-datum $datum-argument)
    (lets
      ((argument $key-datums $value-datum) $datum-argument)
      `(
        ,(cond
          ((single? $key-datums) (car $key-datums))
          (else `(values ,@$key-datums)))
        ,$value-datum)))

  (function (arguments-lets-datum $datum-arguments $fn)
    `(lets
      ,@(map argument-datum $datum-arguments)
      ,($fn (apply append (map argument-keys $datum-arguments)))))

  (function (datum-arguments-application $target-datum $datum-arguments)
    (arguments-lets-datum
      $datum-arguments
      (lambda ($params)
        (datum-application $target-datum $params))))

  (function (datum-arguments-tuple $datum-arguments)
    (arguments-lets-datum
      $datum-arguments
      (lambda ($params)
        (datum-tuple $params))))
)
