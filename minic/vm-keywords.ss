(library (minic vm-keywords)
  (export
    alloc free
    const ld inc dec add sub
    in out
    block switch loop)
  (import (syntax))

  (define-aux-keywords
    alloc free
    const ld inc dec add sub
    in out
    block switch loop)
)
