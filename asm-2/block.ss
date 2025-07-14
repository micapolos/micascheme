(library (asm-2 block)
  (export
    block block? block-size block-org->binary-proc
    block->binary)
  (import (micascheme) (syntax lookup))

  (data (block size org->binary-proc))

  (define (block->binary $block $org)
    ((block-org->binary-proc $block) $org))
)
