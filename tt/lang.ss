(library (tt lang)
  (export
    tt define-type
    (rename (%define define)))
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
            primitive-apply-term
            (native (partial generate-class (symbol->string 'id)))
            arg ...)))))

  (define-syntax (tt $syntax)
    (lambda ($lookup)
      (syntax-case $syntax ()
        ((_ x)
          (term->syntax primitive->syntax 0
            (compile-type $lookup #'x))))))

  (define-syntax (%define $syntax)
    (lambda ($lookup)
      (syntax-case $syntax ()
        ((_ id x)
          (lets
            ($typed (compile-typed $lookup #'x))
            #`(define id
              (make-compile-time-value
                (typed
                  #,(term->syntax primitive->syntax 0 (typed-type $typed))
                  #,(typed-ref $typed)))))))))
)
