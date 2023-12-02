(library (tico constant)
  (export
    constant constant? constant-values
    constant-value
    constants-values
    datum->constant
    bindings-datum->constant
    constant-arity
    constant-application
    constant-abstraction
    constant-tuple
    constant-tuple-ref
    constant-struct
    constant-ref)
  (import
    (micascheme)
    (tico tuple)
    (tico arity)
    (tico index)
    (tico datum)
    (tico value)
    (evaluator))

  (data (constant . values))

  (define constant-environment
    (environment
      '(micascheme)
      '(tico type)
      '(tico tuple)))

  (define (constants-values $constants)
    (apply append (map constant-values $constants)))

  (define (constant-value $constant)
    (force-single (constant-values $constant)))

  (define (constant-arity $constant)
    (arity (length (constant-values $constant))))

  (define (datum->constant $datum)
    (bindings-datum->constant (stack) $datum))

  (define (bindings-datum->constant $bindings $datum)
    (apply constant
      (evaluate
        (evaluator constant-environment $bindings)
        `(call-with-values
          (lambda () ,$datum)
          list))))

  (define (constant-application $target $args)
    (call-with-values
      (lambda ()
        (apply
          (constant-value $target)
          (constants-values $args)))
      constant))

  (define (constant-abstraction $arity $body-constants)
    (lets
      ($body-values (constants-values $body-constants))
      ($body-symbols (generate-symbols (length $body-values)))
      (constant
        (evaluate
          (evaluator
            constant-environment
            (map cons $body-symbols $body-values))
          `(lambda (,@(generate-symbols $arity))
            ,(case (length $body-symbols)
              ((1) (car $body-symbols))
              (else `(values ,@$body-symbols))))))))

  (define (constant-struct $name $field-values)
    (constant
      (value-struct $name
        (map constant-value $field-values))))

  (define (constant-tuple $constants)
    (lets
      ($values (constants-values $constants))
      ($symbols (generate-symbols (length $values)))
      (constant
        (evaluate
          (evaluator
            constant-environment
            (map cons $symbols $values))
          `(tuple ,@$symbols)))))

  (define (constant-ref $arity $target $index)
    (constant
      (value-ref $arity (constant-value $target) $index)))

  (define (constant-tuple-ref $arity $tuple-constant $index)
    (lets
      ($value (constant-value $tuple-constant))
      ($symbol (generate-symbol))
      (constant
        (evaluate
          (evaluator
            constant-environment
            (list (cons $symbol $value)))
          `(tuple-ref
            ,(arity-value $arity)
            ,$symbol
            ,(index-value $index))))))
)
