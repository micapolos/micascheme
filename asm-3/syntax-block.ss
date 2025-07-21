(library (asm-3 syntax-block)
  (export
    db dw align
    syntax->block
    check-syntax->block)
  (import (asm-3 base) (asm-3 block) (asm-3 syntax-expression))

  (define-keywords db dw align)

  (define (syntax->block $syntax)
    (syntax-case $syntax (db dw align begin)
      ((db x ...)
        (list->block
          (map
            (dot u8-expression-block syntax->expression)
            #'(x ...))))
      ((dw x ...)
        (list->block
          (map
            (dot
              (partial-flip u16-expression-block (endianness little))
              syntax->expression)
            #'(x ...))))
      ((align x)
        (align-block (datum x)))
      ((begin x ...)
        (list->block (map syntax->block #'(x ...))))
      (id
        (identifier? #'id)
        (identifier-block #'id))))

  (define-rule-syntax (check-syntax->block org lookup in out)
    (check (equal? (block->datum org lookup (syntax->block #'in)) 'out)))
)
