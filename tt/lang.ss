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

  (define-syntax (define-type $syntax)
    (syntax-case $syntax ()
      ((_ id)
        (identifier? #'id)
        #`(define-syntax id
          (make-compile-time-value
            (generate-declaration
              #,(literal->syntax (symbol->string (datum id)))
              0))))
      ((_ (id arity))
        (and
          (identifier? #'id)
          (integer? (datum arity))
          (nonnegative? (datum arity)))
        #`(define-syntax id
          (make-compile-time-value
            (generate-declaration
              #,(literal->syntax (symbol->string (datum id)))
              arity))))))

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
            #`(define-syntax id
              (make-compile-time-value
                (typed
                  #,(term->syntax primitive->syntax 0 (typed-type $typed))
                  #,(typed-ref $typed)))))))))
)
