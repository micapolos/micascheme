(library (asm-2 core)
  (export
    (rename (%block block))
    db dw)
  (import (micascheme) (asm-2 fragment) (asm-2 block) (asm-2 block-fragment))

  (define-rule-syntax (%block line ...)
    (labeled-block-fragment line ...))

  (define-rule-syntax (db x ...)
    (fragment-with
      (block-append
        (block 1
          (lambda (_)
            (u8-binary x)))
        ...)))

  (define-rule-syntax (dw x ...)
    (fragment-with
      (block-append
        (block 2
          (lambda (_)
            (u16-binary x (endianness little))))
        ...)))
)
