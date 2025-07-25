(library (asm-3 syntax-block)
  (export align syntax->block check-syntax->block)
  (import (asm-3 base) (asm-3 block) (asm-3 syntax-expression))

  (define-keywords align)

  (define (syntax->block $lookup $syntax)
    (syntax-case $syntax (syntax begin align)
      ((begin x ...)
        (list->block (map (partial syntax->block $lookup) #'(x ...))))
      ((align x)
        (integer? (datum x))
        (align-block (datum x)))
      (id
        (identifier? #'id)
        (identifier-block #'id))
      ((id . x)
        (identifier? #'id)
        (switch (transform (lookup-ref $lookup #'id) $syntax $lookup)
          ((block? $block) $block)
          ((else $other) (syntax->block $lookup $other))))
      (other
        (syntax-error $syntax "invalid block syntax"))))

  (define-rule-syntax (check-syntax->block lookup in out)
    (check-block (syntax->block lookup #'in) out))
)
