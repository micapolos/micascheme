(library (asm-3 syntax-fragment)
  (export syntax->fragment)
  (import
    (asm-3 base)
    (asm-3 syntax-block)
    (asm-3 block-fragment))

  (define (syntax->fragment $syntax)
    (block->fragment (syntax->block $syntax)))
)
