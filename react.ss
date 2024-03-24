(library (react)
  (export react)
  (import
    (micascheme)
    (react-impl))
  (export
    (import (react-lib)))

  (define-syntax (react $syntax)
    (syntax-case $syntax ()
      ((_ $item ...)
        (lambda ($lookup)
          (lets
            ($syntax (react-syntax $lookup #`($item ...)))
            #`(begin
              (pretty-print (quote #,$syntax))
              #,$syntax))))))
)
