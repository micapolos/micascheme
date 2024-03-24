(library (sequential)
  (export (rename (sequential-top-level sequential)))
  (import (micascheme) (sequential-syntax))
  (export (import (only (sequential-syntax) sequence)))

  (define-syntax (sequential-top-level $syntax)
    (syntax-case $syntax ()
      ((_ $item ...)
        (lambda ($lookup)
          (syntax-list-transform
            (lookup-context
              (lambda ($id)
                ($lookup $id #`sequential)))
            (syntax->list #`($item ...)))))))
)
