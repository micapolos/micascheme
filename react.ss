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
        ((_ $item ...) (react-syntax #`($item ...))))))
)
