(library (react)
  (export react)
  (import
    (micascheme)
    (react-impl))
  (export
    (import (react-lib)))

  (define-syntax react
    (lambda ($syntax)
      (syntax-case $syntax ()
        ((_ $item ...)
          (lambda ($lookup)
            (react-syntax $lookup #`($item ...)))))))
)
