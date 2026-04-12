(library (leo assert)
  (export
    assertion-violation
    assert)
  (import
    (rename (scheme)
      (assert %assert)
      (assertion-violation %assertion-violation))
    (syntax)
    (condition))

  (define-rule-syntax (assertion-violation cause)
    (raise
      (condition
        (make-assertion-violation)
        (make-cause-condition cause))))

  (define-rule-syntax (assert expr)
    (unless expr
      (assertion-violation 'expr)))
)
