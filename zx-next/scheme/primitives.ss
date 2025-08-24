(library (zx-next scheme primitives)
  (export byte-zero)
  (import
    (only (micascheme) define-syntax make-compile-time-value syntax)
    (zx-next scheme prims)
    (zx-next scheme value))

  (define-syntax byte-zero
    (make-compile-time-value
      #'(load-value (byte-value #x00))))

  ; TODO: Define all primitives
)
