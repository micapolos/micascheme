(library (tico constant)
  (export
    constant constant? constant-value
    datum->constant
    constant-application)
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
)
