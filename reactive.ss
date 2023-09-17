(library (reactive)
  (export (rename (reactive-top-level reactive)))
  (import (micascheme) (reactive-syntax))

  (define-syntax reactive-top-level
    (lambda ($syntax)
      (syntax-case $syntax ()
        ((_ $item ...)
          (lambda ($lookup)
            (syntax-list-transform
              (context
                (lambda ($id)
                  ($lookup $id #`reactive)))
              (syntax->list #`($item ...))))))))
)
