(library (asm-3 syntax-block)
  (export syntax->block check-syntax->block)
  (import (asm-3 base) (asm-3 block) (asm-3 syntax-expression))

  (define (syntax->block $lookup $syntax)
    (syntax-case $syntax (syntax begin)
      ((begin x ...)
        (list->block (map (partial syntax->block $lookup) #'(x ...))))
      (id
        (identifier? #'id)
        (identifier-block #'id))
      ((id . x)
        (identifier? #'id)
        (switch ((lookup-ref $lookup #'id) $lookup $syntax)
          ((syntax? $syntax) (syntax->block $lookup $syntax))
          ((block? $block) $block)
          ((else $other) (syntax-error $syntax "invalid block syntax"))))
      (other
        (syntax-error $syntax "invalid block syntax"))))

  (define-rule-syntax (check-syntax->block lookup org dependency-lookup in out)
    (check-block org dependency-lookup (syntax->block lookup #'in) out))
)
