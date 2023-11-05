(library (tico datum)
  (export
    datum-application
    generate-datum-params
    datum-abstraction
    generate-datum-abstraction
    lets-datum
    datum-tuple
    tuple-ref-datum
    value->datum
    datum->value
    datum-struct)
  (import (micascheme) (tico type))

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

  (define (datum-tuple $items)
    (case (length $items)
      ((0) (throw error))
      ((1) (car $items))
      ((2) `(cons ,(car $items) ,(cadr $items)))
      (else `(vector ,@$items))))

  (define (datum-struct $name $field-datums)
    (datum-tuple $field-datums))

  (define (lets-datum $declarations $body)
    (switch $declarations
      ((null? _) $body)
      ((pair? $pair) `(lets ,@$pair ,$body))))

  (define (tuple-ref-datum $arity $tuple $index)
    (case $arity
      ((0) (throw error))
      ((1) $tuple)
      ((2) `(,(if (zero? $index) `car `cdr) ,$tuple))
      (else `(vector-ref ,$tuple ,$index))))

  (define datum-environment
    (environment '(micascheme) '(tico type)))

  (define (datum->value $datum)
    (eval $datum datum-environment))

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
      ((native-type? _)
        `(native-type))
      ((native-type? _)
        `(native-type))
      ((value-type? $value-type)
        `(value-type ,(value->datum (value-type-value $value-type))))
      ((struct? $struct)
        `(struct
          ,(value->datum (struct-name $struct))
          (list ,@(map value->datum (struct-fields $struct)))))
      ((arrow? $arrow)
        `(arrow
          (list ,@(map value->datum (arrow-params $arrow)))
          ,(value->datum (arrow-result $arrow))))
      ((else $other)
        (throw value->datum $value))))
)
