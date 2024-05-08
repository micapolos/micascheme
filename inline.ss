(library (inline)
  (export
    inline
    inline-datum
    inline-bytevector
    inline-vector)
  (import (scheme) (syntax) (syntaxes) (procedure))

  (define-case-syntaxes (literals import)
    ((inline (import import-spec ...) $syntax)
      (eval
        (syntax->datum/annotation #'$syntax)
        (apply environment
          (map syntax->datum
            (syntax->list #'(import-spec ...)))))))

  (define-rules-syntax (literals import)
    ((inline-datum (import import-spec ...) $expr)
      (inline (import (scheme) import-spec ...)
        (datum->syntax (car (generate-temporaries '(tmp)))
          $expr))))

  (define-rules-syntax (literals import)
    ((inline-bytevector (import import-spec ...) $expr)
      (inline (import (syntax) import-spec ...)
        (bytevector->syntax $expr))))

  (define-rules-syntax (literals import)
    ((inline-vector (import import-spec ...) $expr)
      (inline (import (syntax) import-spec ...)
        (vector->syntax $expr))))
)
