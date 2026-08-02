(library (tt lang)
  (export
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

  (define-syntax (define-class $syntax)
    (syntax-case $syntax ()
      ((_ id)
        (identifier? #'id)
        #`(define-class (id)))
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
      (syntax-case $syntax (%class)
        ((_ (%class . x))
          #'(define-class . x))
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
                    #'#,(typed-ref $typed))))))))))
)
