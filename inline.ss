(library (inline)
  (export
    inline
    inline-bytevector)
  (import (scheme) (syntax) (syntaxes) (procedure))

  (define-rules-syntax (literals import)
    ((inline-bytevector (import import-spec ...) $expr)
      (inline (import (syntax) import-spec ...)
        (bytevector->syntax $expr))))

  (define-case-syntaxes (literals import)
    ((inline $syntax)
      (eval
        (syntax->datum/annotation #'$syntax)))
    ((inline (import import-spec ...) $syntax)
      (eval
        (syntax->datum/annotation #'$syntax)
        (apply environment
          (map syntax->datum
            (syntax->list #'(import-spec ...)))))))
)
