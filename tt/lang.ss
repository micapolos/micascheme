(library (tt lang)
  (export
    tt define-type)
  (import
    (scheme)
    (syntax)
    (syntaxes)
    (lets)
    (procedure)
    (tt hoas)
    (tt primitive)
    (tt hoas-compiler))
  (export (import (tt keywords)))

  (define-rules-syntax
    ((define-type id)
      (identifier? #'id)
      (define-type (id)))
    ((define-type (id arg ...))
      (identifier? #'id)
      (define-syntax id
        (make-compile-time-value
          (native-abstraction
            (partial generate-class (symbol->string 'id))
            arg ...)))))

  (define-syntax (tt $syntax)
    (lambda ($lookup)
      (syntax-case $syntax ()
        ((_ x)
          (term->syntax primitive->syntax 0
            (compile-type $lookup #'x))))))
)
