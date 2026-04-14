(library (leo error)
  (export
    error
    assertion-violation
    assert)
  (import
    (rename (scheme)
      (error %error)
      (assert %assert)
      (assertion-violation %assertion-violation))
    (syntax)
    (syntaxes)
    (condition))

  (define-rules-syntaxes
    ((error cause)
      (raise
        (condition
          (make-error)
          (make-cause-condition cause))))
    ((assertion-violation cause)
      (raise
        (condition
          (make-assertion-violation)
          (make-cause-condition cause))))
    ((assert expr)
      (unless expr
        (assertion-violation 'expr))))
)
