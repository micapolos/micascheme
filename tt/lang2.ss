(library (tt lang2)
  (export
    define-global
    define-primitive
    (rename
      (%print print)
      (%define define)))
  (import
    (scheme)
    (syntax)
    (syntaxes)
    (boolean)
    (lets)
    (procedure)
    (check)
    (switch)
    (list-syntax)
    (keyword)
    (tt term)
    (tt primitive)
    (tt compiler)
    (tt type)
    (prefix (only (scheme) not) %)
    (prefix (tt keywords) %))
  (export (import (tt keywords)))

  (define-syntax (define-global $syntax)
    (lambda ($lookup)
      (compile-define-global $lookup $syntax)))

  (define-syntax (%define $syntax)
    (lambda ($lookup)
      (syntax-case $syntax (%forall)
        ((_ (id (%forall t ...) param ...) body)
          (identifier? #'id)
          #`(%define id (%forall (t ...) (%lambda (param ...) body))))
        ((_ (id param ...) body)
          (identifier? #'id)
          #`(%define id (%lambda (param ...) body)))
        ((_ id x)
          (identifier? #'id)
          (lets
            ($typed-value (compile-typed-value $lookup #'x))
            ($typed-syntax
              #`(typed
                #,(type->syntax (typed-type $typed-value))
                #,(type->syntax (typed-ref $typed-value))))
            (switch ($lookup #'id)
              ((false? _)
                #`(define-syntax id
                  (make-compile-time-value
                    (typed-value-box #,$typed-syntax))))
              ((else _)
                #`(define-property id typed-value-box
                  (typed-value-box #,$typed-syntax)))))))))

  (define-syntax (define-primitive $syntax)
    (lambda ($lookup)
      (syntax-case $syntax ()
        ((_ id (prim-id (param ...) result))
          (with-syntax
            ((((param-id param-type) ...)
              (map-with
                ($tmp (generate-temporaries #'(param ...)))
                ($param #'(param ...))
                #`(#,$tmp #,$param))))
            #`(begin
              (define-global global-id prim-id)
              (%define (id (param-id param-type) ...)
                (%call result global-id param-id ...))))))))

  (define-syntax (%print $syntax)
    (lambda ($lookup)
      (syntax-case $syntax ()
        ((_ x)
          (lets
            ($typed-value (compile-typed-value $lookup #'x))
            #`(pretty-print
              `(compiled
                (type #,(literal->syntax (type->datum (typed-type $typed-value))))
                (value #,(literal->syntax (type->datum (typed-ref $typed-value)))))))))))
)
