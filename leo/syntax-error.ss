(library (leo syntax-error)
  (export syntax-error)
  (import
    (rename (scheme) (syntax-error %syntax-error))
    (condition)
    (syntaxes))

  (define-rules-syntax
    ((syntax-error stx)
      (raise
        (condition
          (make-syntax-violation stx #f))))
    ((syntax-error stx cause)
      (raise
        (condition
          (make-syntax-violation stx #f)
          (make-cause-condition cause))))
    ((syntax-error stx cause hint)
      (raise
        (condition
          (make-syntax-violation stx #f)
          (make-cause-condition cause)
          (make-hint-condition hint)))))
)
