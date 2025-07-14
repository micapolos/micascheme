(library (asm-2 block)
  (export block block->blob)
  (import (micascheme) (syntax lookup))

  (define-rule-syntax (block ($org) blob)
    (lambda ($org) blob))

  (define (block->blob $block $org)
    ($block $org))
)
