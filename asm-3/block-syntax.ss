(library (asm-3 block-syntax)
  (export
    label
    db
    dw-le dw-be
    align
    begin)
  (import
    (except (asm-3 base) begin)
    (asm-3 block)
    (asm-3 expression)
    (asm-3 expression-syntax))

  (define-rule-syntax (label id)
    (identifier-block #'id))

  (define-rule-syntax (db x ...)
    (block-append
      (u8-expression-block (expr x))
      ...))

  (define-rule-syntax (dw-le x ...)
    (block-append
      (u16-expression-block (expr x) (endianness little))
      ...))

  (define-rule-syntax (dw-be x ...)
    (block-append
      (u16-expression-block (expr x) (endianness big))
      ...))

  (define-rule-syntax (align x)
    (align-block (datum x)))

  (define-rule-syntax (begin x ...)
    (block-append x ...))
)
