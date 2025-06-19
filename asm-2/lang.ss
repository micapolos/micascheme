(library (asm-2 lang)
  (export asm)
  (import (micascheme) (asm-2 typed))

  (define-syntax (asm $syntax $lookup)
    (syntax-case $syntax ()
      ((_ expr)
        (typed-value
          (syntax->typed $lookup #'expr)))))
)
