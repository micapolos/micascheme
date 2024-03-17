(library (tipsy)
  (export
    tipsy
    tipsy-expr
    define-expr
    define-type
    define-tipsy)
  (import
    (micascheme)
    (tipsy-expr))

  (define-syntax tipsy
    (lambda ($syntax)
      (lambda ($lookup)
        (syntax-case $syntax ()
          ((_ $expr)
            (expr-syntax
              (syntax-expr
                (lambda ($identifier)
                  ($lookup $identifier #'expr))
                #'$expr)))))))

  (define-syntax tipsy-expr
    (lambda ($syntax)
      (lambda ($lookup)
        (syntax-case $syntax ()
          ((_ $expr)
            (expr-self-syntax
              (syntax-expr
                (lambda ($identifier)
                  ($lookup $identifier #'expr))
                #'$expr)))))))

  (define-syntax-rule (define-expr $identifier $expr)
    (define-property $identifier expr $expr))

  (define-syntax-rule (define-type $identifier $type)
    (define-expr $identifier (expr #'$type #'$identifier)))

  (define-syntax define-tipsy
    (lambda ($syntax)
      (lambda ($lookup)
        (syntax-case $syntax ()
          ((_ $id $body)
            (lets
              ($expr
                (syntax-expr
                  (lambda ($identifier)
                    ($lookup $identifier #'expr))
                  #'$body))
              (with-implicit ($id untyped)
                #`(begin
                  (define untyped #,(expr-value $expr))
                  (define-syntax $id
                    (lambda ($syntax)
                      #`(syntax-error #'#,$syntax "misplaced tipsy")))
                  (define-expr $id
                    (expr
                      #'#,(expr-type $expr)
                      #'untyped))))))))))
)
