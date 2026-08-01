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
    (tt hoas-compiler)
    (prefix (tt keywords) %))
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
          (lets
            ($typed (compile-typed $lookup #'x))
            #`(check
              (equal?
                `(typed
                  #,(literal->syntax
                    (term->datum primitive->datum 0 (typed-type $typed)))
                  ,#,(typed-ref $typed))
                'd)))))))

  (define-syntax (%define $syntax)
    (lambda ($lookup)
      (syntax-case $syntax ()
        ((_ id x)
          (identifier? #'id)
          (lets
            ($typed (compile-typed $lookup #'x))
            #`(begin
              (define untyped #'#,(typed-ref $typed))
              (define-syntax id
                (make-compile-time-value
                  (typed
                    #,(term->syntax primitive->syntax 0 (typed-type $typed))
                    #'#,(typed-ref $typed)))))))
        ((_ (id param ... result) x)
          #`(%define id
            (%typed
              (%-> param ... result)
              x))))))
)
