(library (tico constant)
  (export
    constant constant? constant-value
    datum->constant
    constant-application
    constant-struct
    constant-ref)
  (import
    (micascheme)
    (tico datum)
    (tico value))

  (data (constant value))

  (define (datum->constant $datum)
    (constant (datum->value $datum)))

  (define (constant-application $target $args)
    (constant
      (value-application
        (constant-value $target)
        (map constant-value $args))))

  (define (constant-struct $name $field-values)
    (constant
      (value-struct $name
        (map constant-value $field-values))))

  (define (constant-ref $arity $target $index)
    (constant
      (value-ref $arity (constant-value $target) $index)))
)
