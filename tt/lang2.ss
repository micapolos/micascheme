(library (tt lang2)
  (export
    define-global
    (rename
      (%print print)))
  (import
    (scheme)
    (syntax)
    (syntaxes)
    (lets)
    (procedure)
    (check)
    (switch)
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
