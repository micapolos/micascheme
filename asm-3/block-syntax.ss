(library (asm-3 block-syntax)
  (export
    label
    u8
    u16-le u16-be
    align
    begin)
  (import
    (except (asm-3 base) begin)
    (asm-3 block)
    (asm-3 expression)
    (asm-3 expression-syntax))

  (define-rule-syntax (label id)
    (identifier-block #'id))

  (define-rule-syntax (u8 x)
    (block-append
      (u8-expression-block (expr x))))

  (define-rule-syntax (u16-le x)
    (block-append
      (u16-expression-block (expr x) (endianness little))))

  (define-rule-syntax (u16-be x)
    (block-append
      (u16-expression-block (expr x) (endianness big))))

  (define-rule-syntax (align x)
    (align-block (datum x)))

  (define-rule-syntax (begin x ...)
    (block-append x ...))
)
