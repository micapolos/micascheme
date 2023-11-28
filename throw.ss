(library (throw)
  (export ensure throw or-throw)
  (import
    (scheme)
    (lets)
    (syntax))

  (define-syntax-rule (throw name item ...)
    (error #f (format "~s" (list (quote name) item ...))))

  (define-syntax or-throw
    (syntax-rules ()
      ((_ ($target $arg ...))
        (or
          ($target $arg ...)
          (throw or-throw ($target $arg ...))))
      ((_ $other)
        (or $other
          (throw or-throw $other)))))

  (define-syntax-rule (ensure $pred $expr)
    (lets
      ($tmp $expr)
      (cond
        (($pred $tmp) $tmp)
        (else (throw ensure (quote $pred) $tmp)))))
)
