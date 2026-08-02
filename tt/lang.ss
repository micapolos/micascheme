(library (tt lang)
  (export
    define-class
    define-macro
    (rename
      (%define define)
      (%define-record define-record)
      (%check check)
      (%print print)))
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
    (compile-define-class $syntax))

  (define-syntax (%define-record $syntax)
    (lambda ($lookup)
      (compile-define-record $lookup $syntax)))

  (define-syntax (define-macro $syntax)
    (compile-define-macro $syntax))

  (define-syntax (%check $syntax)
    (lambda ($lookup)
      (syntax-case $syntax ()
        ((_ (eq? a b))
          (lets
            ($typed-a (compile-typed $lookup #'a))
            ($type (typed-type $typed-a))
            ($a (typed-ref $typed-a))
            ($b (compile-value $lookup $type #'b))
            ($eq? (compile-value $lookup (arrow (list $type $type) boolean-type) #'eq?))
            #`(check (#,$eq? #,$a #,$b))))
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
      (compile-define $lookup $syntax)))

  (define-syntax (%print $syntax)
    (lambda ($lookup)
      (compile-print $lookup $syntax)))
)
