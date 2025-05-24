(library (zasm lang)
  (export
    zasm)
  (import
    (micascheme)
    (zasm syntax)
    (zasm context))
  (export
    (import (zasm keywords)))

  (define-syntax (zasm $syntax $lookup)
    (syntax-case $syntax ()
      ((_ op ...)
        #`(lambda ($context)
          #,(zasm-transform $lookup #'(begin op ...))))))
)
