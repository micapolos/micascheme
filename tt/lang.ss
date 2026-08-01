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
    (switch)
    (tt hoas)
    (tt primitive)
    (tt hoas-compiler)
    (prefix (tt keywords) %))
  (export (import (tt keywords)))

  (define-syntax (define-type $syntax)
    (syntax-case $syntax ()
      ((_ id)
        (identifier? #'id)
        #`(define-type (id)))
      ((_ (id param ...))
        (for-all identifier? #'(id param ...))
        #`(define-syntax id
          (make-compile-time-value
            (generate-declaration
              #,(literal->syntax (symbol->string (datum id)))
              #,(literal->syntax (length #'(param ...)))))))))

  (define-syntax (%check $syntax)
    (lambda ($lookup)
      (syntax-case $syntax ()
        ((_ x d)
          (switch (compile-compiled $lookup #'x)
            ((type? _) (todo))
            ((typed? $typed)
              #`(check
                (equal?
                  `(typed
                    #,(literal->syntax
                      (term->datum primitive->datum 0 (typed-type $typed)))
                    ,#,(typed-ref $typed))
                  'd))))))))

  (define-syntax (%define $syntax)
    (lambda ($lookup)
      (syntax-case $syntax ()
        ((_ id x)
          (identifier? #'id)
          (lets
            ($compiled (compile-compiled $lookup #'x))
            #`(begin
              (define untyped #'#,(typed-ref $compiled))
              (define-syntax id
                (make-compile-time-value
                  (typed
                    #,(term->syntax primitive->syntax 0 (typed-type $compiled))
                    #'#,(typed-ref $compiled)))))))
        ((_ (id param ... result) x)
          #`(%define id
            (%typed
              (%-> param ... result)
              x))))))
)
