(library (tico constant)
  (export
    constant constant? constant-values
    constant-value
    constants-values
    datum->constant
    bindings-datum->constant
    constant-arity
    constant-slice
    constant-application
    constant-abstraction
    constant-struct
    constant-ref)
  (import
    (micascheme)
    (tico arity)
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
        `(call-with-values (lambda () ,$datum) list))))

  (define (constant-slice . $constants)
    (constant (apply slice (map constant-value $constants))))

  (define (constant-application $target $args)
    (constant
      (value-application
        (constant-value $target)
        (map constant-value $args))))

  (define (constant-abstraction $arity $body)
    (constant
      (value-abstraction $arity (constant-value $body))))

  (define (constant-struct $name $field-values)
    (constant
      (value-struct $name
        (map constant-value $field-values))))

  (define (constant-ref $arity $target $index)
    (constant
      (value-ref $arity (constant-value $target) $index)))
)
