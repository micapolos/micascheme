(library (tico argument)
  (export
    argument argument? argument-values
    argument-value
    arguments-values
    datum->argument
    bindings-datum->argument
    argument-arity
    argument-application
    argument-application-2
    argument-abstraction
    argument-abstraction-2
    argument-tuple
    argument-tuple-ref
    argument-struct
    argument-ref)
  (import
    (micascheme)
    (tico tuple)
    (tico arity)
    (tico index)
    (tico datum)
    (tico value)
    (evaluator))

  (data (argument . values))

  (define argument-environment
    (environment
      '(micascheme)
      '(tico type)
      '(tico tuple)))

  (define (arguments-values $arguments)
    (apply append (map argument-values $arguments)))

  (define (argument-value $argument)
    (force-single (argument-values $argument)))

  (define (argument-arity $argument)
    (arity (length (argument-values $argument))))

  (define (datum->argument $datum)
    (bindings-datum->argument (stack) $datum))

  (define (bindings-datum->argument $bindings $datum)
    (apply argument
      (evaluate
        (evaluator argument-environment $bindings)
        `(call-with-values
          (lambda () ,$datum)
          list))))

  (define (argument-application $target $args)
    (argument
      (value-application
        (argument-value $target)
        (map argument-value $args))))

  (define (argument-application-2 $target $args)
    (lets
      ($values (arguments-values (cons $target $args)))
      (do (unless (pair? $values) (throw argument-application-2 $target $args)))
      ($target (car $values))
      ($args (cdr $values))
      (call-with-values
        (lambda () (apply $target $args))
        argument)))

  (define (argument-abstraction $arity $body)
    (argument
      (value-abstraction $arity (argument-value $body))))

  (define (argument-abstraction-2 $arity $body-arguments)
    (lets
      ($body-values (arguments-values $body-arguments))
      ($body-symbols (generate-symbols (length $body-values)))
      (argument
        (evaluate
          (evaluator
            argument-environment
            (map cons $body-symbols $body-values))
          `(lambda (,@(generate-symbols $arity))
            ,(case (length $body-symbols)
              ((1) (car $body-symbols))
              (else `(values ,@$body-symbols))))))))

  (define (argument-struct $name $field-values)
    (argument
      (value-struct $name
        (map argument-value $field-values))))

  (define (argument-tuple $arguments)
    (lets
      ($values (arguments-values $arguments))
      ($symbols (generate-symbols (length $values)))
      (argument
        (evaluate
          (evaluator
            argument-environment
            (map cons $symbols $values))
          `(tuple ,@$symbols)))))

  (define (argument-ref $arity $target $index)
    (argument
      (value-ref $arity (argument-value $target) $index)))

  (define (argument-tuple-ref $arity $tuple-argument $index)
    (lets
      ($value (argument-value $tuple-argument))
      ($symbol (generate-symbol))
      (argument
        (evaluate
          (evaluator
            argument-environment
            (list (cons $symbol $value)))
          `(tuple-ref
            ,(arity-value $arity)
            ,$symbol
            ,(index-value $index))))))
)
