(library (tico constant)
  (export
    constant constant? constant-value
    datum->constant)
  (import
    (micascheme)
    (tico datum))

  (data (constant value))

  (define (datum->constant $datum)
    (constant (datum->value $datum)))
)
