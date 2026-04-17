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
    ((error causes ...)
      (raise
        (condition
          (make-error)
          (make-cause-condition causes)
          ...)))
    ((assertion-violation causes ...)
      (raise
        (condition
          (make-assertion-violation)
          (make-cause-condition causes) ...)))
    ((assert expr)
      (unless expr
        (assertion-violation 'expr))))
)
