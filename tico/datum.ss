(library (tico datum)
  (export
    datum-application
    datum-abstraction
    arity-abstraction
    datum-tuple
    arity-datum-ref
    value->datum
    datum->value)
  (import (micascheme) (tico type))

  (define (arity-abstraction $arity $fn)
    (lets
      ($params (generate-symbols $arity))
      (datum-abstraction $params ($fn $params))))

  (define (datum-abstraction $params $body)
    `(lambda (,@$params) ,$body))

  (define (datum-application $target $args)
    `(,$target ,@$args))

  (define (datum-tuple $items)
    (case (length $items)
      ((0) #f)
      ((1) (car $items))
      ((2) `(cons ,(car $items) ,(cadr $items)))
      (else `(vector ,@$items))))

  (define (arity-datum-ref $arity $datum $index)
    (case $arity
      ((0) '(throw error))
      ((1) $datum)
      ((2) `(,(if (zero? $index) `car `cdr) ,$datum))
      (else `(vector-ref ,$datum ,$index))))

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
