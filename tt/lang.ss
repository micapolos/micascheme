(library (tt lang)
  (export
    define-type
    (rename
      (%define define)
      (%check check)))
  (import
    (scheme)
    (syntax)
    (syntaxes)
    (lets)
    (procedure)
    (check)
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

  (define-syntax (%check $syntax)
    (lambda ($lookup)
      (syntax-case $syntax ()
        ((_ x d)
          #`(check
            (equal?
              '#,(literal->syntax
                (typed->datum
                  (compile-typed $lookup #'x)))
              'd))))))

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
