(library (reactive)
  (export (rename (reactive-top-level reactive)))
  (import (micascheme) (reactive-syntax))
  (export (import (only (reactive-syntax) pure value)))

  (define-syntax reactive-top-level
    (lambda ($syntax)
      (syntax-case $syntax ()
        ((_ $item ...)
          (lambda ($lookup)
            (syntax-list-transform
              (lookup-context
                (lambda ($id)
                  ($lookup $id #`reactive)))
              (syntax->list #`($item ...))))))))
)
