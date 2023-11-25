(library (tico constant)
  (export
    constant constant? constant-value
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
    (tico datum)
    (tico value))

  (data (constant value))

  (define (constant-arity $constant)
    (value-arity (constant-value $constant)))

  (define (datum->constant $datum)
    (bindings-datum->constant (stack) $datum))

  (define (bindings-datum->constant $bindings $datum)
    (constant
      (bindings-datum->value $bindings $datum)))

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
